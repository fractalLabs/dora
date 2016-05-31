#! /bin/bash


# -----------------------
# Download file
# -----------------------
# wget  $1 --output-document tmp.csv
cat $1 > tmp.csv

# Llevar registro antes y después de modificaciones
cp tmp.csv tmp1.csv

# -----------------------
# Correct encoding
# -----------------------
encode=$(file -i tmp.csv | awk -F '=' '{print $2}')
file="correct_file.csv"
iconv -f $encode -t utf8 tmp.csv | grep -vE '^$' | sed -r 's/(\$[0-9]+,[0-9]+)/"\1"/g' > $file
ssconvert $file $file

# -----------------------
# Metadata display
# -----------------------
echo "{\"metadata\":[{\"file_name\":\"$1\"},{\"encoding\":[{\"origin\": \"$encode\"},{\"destiny\": \"utf-8\"}]},{\"size\":[{\"bytes\":\"$(wc -c tmp.csv | sed 's/tmp.csv//g')\"},{\"characters\":\"$(wc -m tmp.csv | sed 's/tmp.csv//g')\"},{\"words\": \"$(wc -w tmp.csv | sed 's/tmp.csv//g')\"},{\"lines\": \"$(wc -l tmp.csv | sed 's/tmp.csv//g')\"},{\"columns\":\"$(cat tmp.csv | awk -F ',' '{print NF}' | head -n 1)\"}]},{\"aditional_info\":[{\"empty_lines\": \"$(cat tmp.csv | grep -E '^$' | wc -l)\"},{\"format_modifications\": \"$(diff correct_file.csv tmp1.csv | wc -l)\"}]}]}" # > metadata.txt


# -----------------------
# Errase incorrect file
# -----------------------
rm tmp.csv
rm tmp1.csv
