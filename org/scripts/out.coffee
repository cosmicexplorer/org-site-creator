###
read a format string with %s and %q specifiers. no escaped quotes allowed in the
format strings, use %q to show a quote

this is not production code
###
readFormatString = (str) ->
  matches = str?.match /^\(format\s*"([^"]*)"\s*(.*)\)$/i
  if not matches then null else
    fmtStr = matches[1]
    args = matches[2].split('"').filter (s) -> not s.match /^\s*$/
    ind = 0
    argsInd = 0
    res = fmtStr.indexOf '%', ind
    while res isnt -1
      upToIndex = res - 1
      upToIndex = 0 if upToIndex < 0
      if ind is 0 and res is 0
        beforeStr = ""
      else
        beforeStr = fmtStr[0..upToIndex]
      switch fmtStr.charAt res + 1
        when 's'
          fmtStr = beforeStr + args[argsInd] + fmtStr[(res+2)..]
          ind = res + args[argsInd]
          ++argsInd
        when 'q'
          fmtStr = beforeStr + '"' + fmtStr[(res+2)..]
          ind = res + 1
      res = fmtStr.indexOf '%', ind
    fmtStr

# hopefully the web scrapers won't spam me as easily
document.addEventListener 'DOMContentLoaded', (ev) ->
  formatStringLinks = document.querySelectorAll('a.format_eval')
  for i in [0..(formatStringLinks.length - 1)]
    el = formatStringLinks[i]
    el.href = "mailto:#{readFormatString(el.innerHTML)}"
