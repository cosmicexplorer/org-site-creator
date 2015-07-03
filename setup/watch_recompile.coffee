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

# get the output file(s) from the result of an org publish operation. this
# applies for both org files, which are transformed to html, and static files,
# which remain the same
getOrgPublishOutputFile = (inDir, infilePath, outDir, cb) ->
  fullPath = path.resolve infilePath
  infileBasename = path.basename infilePath, path.extname(infilePath)
  fullInDirPath = path.resolve inDir
  relPath = fullPath.replace new RegExp("^#{fullInDirPath}/", "g"), ""
  outPath = path.join outDir, relPath
  outPathDir = path.dirname outPath
  fs.readdir outPathDir, (err, files) ->
    throw err if err
    files.map((file) -> path.resolve path.join(outPathDir, file)).filter((f) ->
      baseName = path.basename f, path.extname(f)
      baseName.match new RegExp infileBasename).forEach cb

fs.watch inputDir, recursive: true, (ev, file) ->
  filePath = path.join inputDir, file
  fs.stat filePath, (err, stats) ->
    if not err and stats.isFile()
      return if recentlyModified[filePath]
      recentlyModified[filePath] = yes
      setupProc emacsScript, [htmlize, inputDir, outputDir], ->
        setTimeout((-> recentlyModified[filePath] = no),
          recentModificationsTimeout)
    else
      getOrgPublishOutputFile inputDir, filePath, outputDir, (file) ->
        fs.unlink file, (unlinkErr) ->
          throw unlinkErr if unlinkErr
          console.error "removed #{file} upon deletion of #{filePath}"

# org-publish leaves around these files with tildes and i hate it
filesToDestroyRegex = /~$/

fs.watch outputDir, recursive: true, (ev, file) ->
  filePath = path.join outputDir, file
  fs.stat filePath, (err, stats) ->
    if not err
      if filePath.match filesToDestroyRegex
        fs.unlink filePath # triggers fs.watch
      else
        return if recentlyModified[filePath]
        recentlyModified[filePath] = yes
        console.log "file #{filePath} updated"
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
