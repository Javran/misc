const _ = require('lodash')
const {readdirSync} = require('fs')
const {readJsonSync, writeJsonSync} = require('fs-extra')
const {join} = require('path-extra')

if (process.argv.length !== 4) {
  console.log(`node merger <storer-instance-dir> <output-file>`)
  process.exit(1)
}

const [_ignored1, _ignored2, instanceDir, outputFilePath] = process.argv

console.log(`instance dir: ${instanceDir}`)
console.log(`output file: ${outputFilePath}`)

const tableIds = readJsonSync(join(instanceDir, 'table-ids.json'))

const grouppedFiles =
  _.mapValues(
    _.groupBy(
      _.flatMap(
        readdirSync(instanceDir),
        // fName: string -> {fileName, tableName, timestamp}
        fName => {
          const reResult = /^(\S+)-(\d+).raw$/.exec(fName)
          if (reResult) {
            const [_ignored3, tableName, tsRaw] = reResult
            return [{fileName: fName, tableName, timestamp: Number(tsRaw)}]
          } else {
            return []
          }
        }
      ),
      'tableName'
    ),
    xs => _.sortBy(xs, 'timestamp').map(x => x.fileName)
  )

const merged = _.mapValues(
  grouppedFiles,
  fileNames =>
    _.flatMap(fileNames, fileName =>
      readJsonSync(join(instanceDir,fileName))
    )
)

// verify table name & uniqueness
_.keys(merged).map(tableName => {
  if (!(tableName in tableIds)) {
    console.error(`invalid tableName: ${tableName}`)
    return
  }

  const idPath = tableIds[tableName]
  const idSet = new Set()
  const xs = merged[tableName]
  if (!Array.isArray(xs)) {
    console.error(`unexpected value for ${tableName}`)
    return
  }

  xs.map(x => {
    const id = _.get(x, idPath)
    if (id) {
      if (idSet.has(id)) {
        console.error(`duplicated id: ${id}`)
      }
      idSet.add(id)
    } else {
      console.error(`invalid id: ${id}`)
    }
  })

})

console.log(`id check done`)
writeJsonSync(outputFilePath, merged)
