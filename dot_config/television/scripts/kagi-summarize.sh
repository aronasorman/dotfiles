#!/bin/bash
# Given a headline from seattle-headlines.sh, extract URL and summarize via Kagi
HEADLINE="$1"
URL=$(~/.config/television/scripts/seattle-headlines.sh | grep -F "$HEADLINE" | cut -f2 | head -1)

if [ -z "$URL" ]; then
    echo "Could not find URL for headline"
    exit 1
fi

echo "🔗 $URL"
echo ""

# Try Kagi summarizer if API key is set
if [ -n "$KAGI_API_KEY" ]; then
    curl -s "https://kagi.com/api/v0/summarize" \
        -H "Authorization: Bot $KAGI_API_KEY" \
        --data-urlencode "url=$URL" \
        -d "summary_type=summary" \
        -d "engine=agnes" \
        | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('data',{}).get('output','No summary available'))"
else
    echo "KAGI_API_KEY not set. Falling back to first paragraphs..."
    echo ""
    curl -s "https://r.jina.ai/$URL" | head -40
fi
