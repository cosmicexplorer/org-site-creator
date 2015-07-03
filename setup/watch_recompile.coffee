#!/usr/bin/env coffee

fs = require 'fs'
path = require 'path'
spawn = require('child_process').spawn

[jekyllServePort, emacsScript, htmlize, inputDir, outputDir] = process.argv[2..]

# used because fs.watch keeps reading it as if the file was changed twice
# whenever the file is saved
recentlyModified = {}
recentModificationsTimeout = 200

childProcs = []

removeProcFromListing = (proc) ->
  childProcs.slice childProcs.indexOf(proc), 1

setupProc = (name, args, cb) ->
  proc = spawn name, args
  childProcs.push proc
  proc.stdout.pipe process.stdout
  proc.stderr.pipe process.stderr
  proc.on 'close', (code) ->
    cb?()
    removeProcFromListing proc

fs.watch inputDir, recursive: true, (ev, file) ->
  filePath = path.join inputDir, file
  return if recentlyModified[filePath]
  recentlyModified[filePath] = yes
  setupProc emacsScript, [htmlize, inputDir, outputDir, filePath], ->
    setTimeout((-> recentlyModified[filePath] = no),
          recentModificationsTimeout)

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
