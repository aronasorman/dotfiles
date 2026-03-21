#!/bin/bash
# Given a headline from seattle-headlines.sh, extract URL and show Jina preview
HEADLINE="$1"
URL=$(~/.config/television/scripts/seattle-headlines.sh | grep -F "$HEADLINE" | cut -f2 | head -1)

if [ -z "$URL" ]; then
    echo "Could not find URL for headline"
    exit 1
fi

echo "🔗 $URL"
echo "📋 Ctrl+O → open in Kagi Summarizer"
echo ""
curl -s "https://r.jina.ai/$URL" | head -40
