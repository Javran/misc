(() => {
  const {_, $} = window

  const taskUploadTableIds = new Promise((resolve, reject) => {
    const tableIds = _.fromPairs(
      KC3Database.con.tables.map(table =>
        [table.name, table.schema.primKey.name]
      )
    )
    const postData = {
      raw: JSON.stringify(tableIds),
    }

    const jqXhr = $.post(
      `http://localhost:19721/post-table-ids/${name}`,
      postData,
      'text'
    )

    jqXhr.done(resp => {
      if (resp !== 'ok') {
        const errMsg = `unexpected response: ${resp}`
        console.error(errMsg)
        return reject(errMsg)
      } else {
        return resolve('ok')
      }
    }).fail(e => {
      console.error(e)
      return reject(e)
    })
  })

  const tableTasks = KC3Database.con.tables.map(async table => {
    const {name} = table
    const xs = await table.toArray()
    const dataChunks = _.chunk(xs,2000)

    const chunkTasks = dataChunks.map((chunk, ind) => new Promise((resolve, reject) => {
      const postData = {
        time: Number(new Date()),
        raw: JSON.stringify(chunk),
      }
      const jqXhr = $.post(
        `http://localhost:19721/post/${name}`,
        postData,
        'text'
      )
      jqXhr.done(resp => {
        if (resp !== 'ok') {
          const errMsg = `unexpected response: ${resp}`
          console.error(errMsg)
          return reject(errMsg)
        } else {
          return resolve('ok')
        }
      }).fail(e => {
        console.error(e)
        return reject(e)
      })
    }))
    return Promise.all(chunkTasks)
  })

  Promise.all([taskUploadTableIds, ...tableTasks]).then(() =>
    console.log('all done')
  )
})()
