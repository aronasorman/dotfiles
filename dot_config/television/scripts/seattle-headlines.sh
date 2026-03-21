#!/bin/bash
# Fetch Seattle Times RSS and output "TITLE\tURL" pairs
curl -s https://www.seattletimes.com/feed/ \
  | python3 -c "
import sys, xml.etree.ElementTree as ET
tree = ET.parse(sys.stdin)
for item in tree.findall('.//item')[:30]:
    title = item.find('title').text or ''
    link = item.find('link').text or ''
    print(f'{title}\t{link}')
"
