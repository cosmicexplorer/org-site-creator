path = require 'path'
spawn = require('child_process').spawn
watch = require 'node-watch'

[jekyllServePort, rootDir, dirsToWatch...] = process.argv[2..]

childProcs = []

removeProcFromListing = (proc) ->
  childProcs.slice childProcs.indexOf(proc), 1

filterByRegex = (reg, out) ->
  (buf) ->
    str = buf.toString()
    out.write str unless str.match reg

setupProc = (name, args, quietRegex, cb) ->
  proc = spawn name, args
  childProcs.push proc
  if quietRegex
    proc.stdout.on 'data', filterByRegex quietRegex, process.stdout
    proc.stderr.on 'data', filterByRegex quietRegex, process.stderr
  else
    proc.stdout.pipe process.stdout
    proc.stderr.pipe process.stderr
  proc.on 'close', (code) ->
    cb?()
    removeProcFromListing proc

remake = (filePath) ->
  console.log "updating from #{filePath}"
  setupProc 'make', ['-C', path.resolve rootDir], /^make/

ignoreFile = (file) ->
  path.basename(file).match /^\.?#/

dirsToWatch.forEach (dir) ->
  watch dir, (file) ->
    filePath = path.join dir, file
    remake filePath unless ignoreFile filePath

openBrowser = ->
  setupProc "#{__dirname}/open_browser_to_url.sh",
    ["http://localhost:#{jekyllServePort}"]

# quit on 'q', open site on 'o'
process.stdin.on 'data', (buf) ->
  str = buf.toString()
  if str.match /^\s*[qQ]/
    process.exit()
  else if str.match /^\s*[oO]/
    openBrowser()
  else
    console.error "couldn't parse keyboard input"

# start jekyll serving
setupProc 'jekyll', ['serve']

# now open the browser (after a short delay)
setTimeout openBrowser, 1000

process.on 'exit', ->
  childProcs.forEach (proc) ->
    proc.kill()
