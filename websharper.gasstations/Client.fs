namespace websharper.gasstations

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.PhoneJS
open IntelliFactory.WebSharper.JQuery
open IntelliFactory.WebSharper.Bing.Maps
open IntelliFactory.WebSharper.Knockout

[<Require(typeof<Resources.AndroidHoloLight>)>]
[<Require(typeof<Resources.GenericStyle>)>]
[<Require(typeof<Resources.IOSDefault>)>]
[<Require(typeof<Resources.TizenWhite>)>]
[<Require(typeof<Resources.Win8White>)>]
[<JavaScript>]
module Client =

    //Getting aroun the need for WebSharper.TypeScript.dll
    [<Inline "$global.ko.observable()">]
    let makeObservable<'a> () = X<KnockoutObservable<'a>>

    [<Inline "$global.ko.observable($value)">]
    let makeObservableVal<'a> (value : 'a) = X<KnockoutObservable<'a>>

    [<Inline "$o($value)">]
    let setObservable (o : KnockoutObservable<'a>) (value : 'a) = X<unit>
    
    [<Inline "$o()">]
    let getObservable (o : KnockoutObservable<'a>) = X<'a>

    [<Inline "$global.ko.computed($0)">]
    let makeComputed (make : unit -> 'a) = X<KnockoutObservable<'a>>

    // Helper for slicing arrays
    module Array =
        let take (n : int) (arr : 'a array) =
            if n <= arr.Length then
                arr.[0..n-1]
            else
                arr.[0..arr.Length-1]
        
    type Station = 
        {
            city           : string
            latitude       : float
            longitude      : float
            station_name   : string
            street_address : string
            zip            : string
        }

    type LoadPanel =
        {
            Visible : KnockoutObservable<bool>
        }

    type MapView =
        {
            Map         : Dom.Element
            SearchQuery : KnockoutSubscribable<string>
            LoadPanel   : LoadPanel
        }

    let bingMapsKey = "Ai6uQaKEyZbUvd33y5HU41hvoov_piUMn6t78Qzg7L1DWY4MFZqhjZdgEmCpQlbe"


    // utility functions and types for computing distances
    module Metric =
        open System

        type Loc = { lat : float; long : float }
        let mkLoc lat long = { lat = lat; long = long }
        let locationToLoc (a : Location) = mkLoc a.Latitude a.Longitude

        let private R = 6371.
        let private toRad (x : float) =
            x * Math.PI / 180.

        let Distance (a : Loc) (b : Loc) =

            let lat1 = toRad a.lat
            let lat2 = toRad b.lat
            let dlat = toRad (b.lat - a.lat)
            let dlong = toRad (b.long - a.long)

            let sinlat = Math.Sin (dlat / 2.)
            let sinlong = Math.Sin (dlong / 2.)

            let a = sinlat * sinlat +
                    Math.Cos lat1 * Math.Cos lat2 *
                    sinlong * sinlong
            let c = 2. * Math.Atan2(Math.Sqrt(a), Math.Sqrt(1.-a))
            R * c

    // Controls for retrieving gas stations and caching them
    module private Stations =
        let private nrelKey = "PMwnNRqcydLTX3KHJBXC31Lf2H2mZaGesNPQsNzr"

        let fetched = makeObservableVal(false)
        let mutable Cache : Station array = [||]

        let private FetchStations () = 
            let url = "https://developer.nrel.gov/api/alt-fuel-stations/v1.json?api_key=" + nrelKey
            Async.FromContinuations(fun (ok, err, ca) ->
                JQuery.GetJSON(url).Then(fun (data : obj) -> 
                    Cache <- As data?fuel_stations
                    setObservable fetched true
                    ok Cache
                    true)|> ignore
            )

        let GetStations () =
            if getObservable(fetched) then
                async { return Cache }
            else 
                FetchStations ()

    let PinClick (box : Infobox) (e : MouseEventArgs) =
        let pin = As<Pushpin> e.Target
        box.SetOptions(InfoboxOptions(Visible = true))

    let InfoClick (e : MouseEventArgs) =
        let box = As<Infobox> e.Target
        box.SetOptions(InfoboxOptions(Visible = false))

    // Adds a Pushpin indicationg a gas station to the map based on its distance to the center of the map
    let AddStations (max : int) (map : Map) =
        async {
            map.Entities.Clear()
            let! stations = Stations.GetStations ()
            let loc = Metric.locationToLoc <| map.GetCenter()
            let radius =  Metric.Distance loc (Metric.locationToLoc <| map.GetBounds().GetNorthwest())
            let locs =
                stations
                |> Array.filter (fun s ->
                    let d = Metric.Distance loc (Metric.mkLoc s.latitude s.longitude)
                    d <= radius
                )
                |> Array.take max

            locs
            |> Array.iter (fun s ->           
                let loc = Location(s.latitude, s.longitude)
                let pin = Pushpin(loc)
                let html =
                    "<h3>" + s.station_name + "</h3>" +
                    "<p>" + s.street_address + "</p>"
                let box = Infobox(loc, InfoboxOptions(Description = html, Visible = false))

                Events.AddHandler(pin, MouseEvent.Click, PinClick box) |> ignore
                Events.AddHandler(box, MouseEvent.Click, InfoClick) |> ignore

                map.Entities.Push pin
                map.Entities.Push box
            )

        }
        |> Async.Start

    // Gets called when searching for an address
    let AddressCallback (map : Map) (response : RestResponse) =
        let coords : float array = response.ResourceSets.[0].Resources.[0]?point?coordinates
        let loc = Location(coords.[0], coords.[1])

        let view = ViewOptions(Center = loc,    
                               Zoom = 12)

        map.SetView(view)

    let SearchAddress (map : Map) (address : string) =
        let addr = Address(Locality = address,
                           CountryRegion = "US")
        
        Rest.RequestLocationByAddress(bingMapsKey, addr, AddressCallback map)
        
    // Listens to map movements and when the view changes refreshes the visible gas stations
    let TracingLocation (map : Map) () =
        let center = map.GetCenter()
        AddStations 75 map

    // Perpares the map
    let MkMap () =
        Div [ Attr.Class "dx-map-container" ]
        |> (fun el ->
            let options = 
                MapOptions(
                    Credentials = bingMapsKey
                )
            let location = Location(40.806862, -96.681679)
            let view = ViewOptions(Center = location,
                                   Zoom = 5)
            let map = Map(el.Body, options)
            map.SetView(view)
            map.SetMapType(MapTypeId.Road)

            Events.AddHandler(map, UnitEvent.Viewchangeend, TracingLocation map) |> ignore

            el, map
        )

    // Binds the view to the model
    let Main =
        Pervasives.ko.bindingHandlers?element <-
            New [
                "update" => fun (element : Dom.Element, valueAccessor) ->
                                let elem = As<Dom.Element> <| Pervasives.ko.unwrap(valueAccessor())
                                JQuery.Of(element).Empty() |> ignore
                                JQuery.Of(element).Append(elem)
            ]

        let MainApp : obj = New []
        MainApp?app <- DevExpress.framework.html.HtmlApplication.Create(
                            As New [
                                "namespace" => MainApp
                                "navigationType" => "simple"
                            ])
        let app = As<DevExpress.framework.html.HtmlApplication.T> MainApp?app

        MainApp?Map <- fun paramz ->
            let sq = makeObservable<string>()
            let searchQuery = sq.extend(As<Misc.extend1<string>> <| New [ "throttle" => 500 ])
            let (domMap, map) = MkMap ()
            let viewModel = 
                {
                    Map = domMap.Dom
                    SearchQuery = searchQuery
                    LoadPanel = { Visible = makeComputed(fun () -> getObservable(Stations.fetched) |> not) }
                }

            searchQuery.subscribe(fun value ->
                let s = value.Trim()
                if not <| System.String.IsNullOrEmpty s then
                    SearchAddress map value
            ) |> ignore
            viewModel

        app.router.register(":view", New ["view" => "Map"])
        app.navigate()
        
