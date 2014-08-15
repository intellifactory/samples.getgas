# creates build/html
rm -r build -errorAction ignore
$d = mkdir build
$d = mkdir build/html
cp -r websharper.gasstations/Content build/html/
cp -r websharper.gasstations/css build/html/
cp -r websharper.gasstations/layouts build/html/
cp -r websharper.gasstations/views build/html/
cp -r websharper.gasstations/js build/html/
cp -r websharper.gasstations/*.css build/html/
cp -r websharper.gasstations/*.html build/html/
cp -r websharper.gasstations/*.png build/html/

