
/*
read a format string with %s and %q specifiers. no escaped quotes allowed in the
format strings, use %q to show a quote

this is not production code
 */
var readFormatString;

readFormatString = function(str) {
  var args, argsInd, beforeStr, fmtStr, ind, matches, res, upToIndex;
  matches = str != null ? str.match(/^\(format\s*"([^"]*)"\s*(.*)\)$/i) : void 0;
  if (!matches) {
    return null;
  } else {
    fmtStr = matches[1];
    args = matches[2].split('"').filter(function(s) {
      return !s.match(/^\s*$/);
    });
    ind = 0;
    argsInd = 0;
    res = fmtStr.indexOf('%', ind);
    while (res !== -1) {
      upToIndex = res - 1;
      if (upToIndex < 0) {
        upToIndex = 0;
      }
      if (ind === 0 && res === 0) {
        beforeStr = "";
      } else {
        beforeStr = fmtStr.slice(0, +upToIndex + 1 || 9e9);
      }
      switch (fmtStr.charAt(res + 1)) {
        case 's':
          fmtStr = beforeStr + args[argsInd] + fmtStr.slice(res + 2);
          ind = res + args[argsInd];
          ++argsInd;
          break;
        case 'q':
          fmtStr = beforeStr + '"' + fmtStr.slice(res + 2);
          ind = res + 1;
      }
      res = fmtStr.indexOf('%', ind);
    }
    return fmtStr;
  }
};

document.addEventListener('DOMContentLoaded', function(ev) {
  var el, formatStringLinks, i, j, ref, results;
  formatStringLinks = document.querySelectorAll('a.format_eval');
  results = [];
  for (i = j = 0, ref = formatStringLinks.length - 1; 0 <= ref ? j <= ref : j >= ref; i = 0 <= ref ? ++j : --j) {
    el = formatStringLinks[i];
    results.push(el.href = "mailto:" + (readFormatString(el.innerHTML)));
  }
  return results;
});
