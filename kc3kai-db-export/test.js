(() => {
  const {_, $} = window

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

  Promise.all(tableTasks).then(() =>
    console.log('all done')
  )
})()
