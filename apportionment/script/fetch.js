(() => {
  const lines = []
  $('#mw-content-text > div.mw-parser-output > table').each((_i,x) => {
    const q = $(x)
    const cTitle = $('caption', q).text()
    if (typeof cTitle === 'string' && cTitle.startsWith('Population')) {
      $('tbody > tr', q).each((_j,r) => {
        const cols = $(r).children().toArray()
        const getRowText = i => $(cols[i]).text().trim()
        const [rankRaw, stateRaw, popRaw] = [0,1,3].map(getRowText)
        const rank = parseInt(rankRaw, 10)
        if (!Number.isNaN(rank)) {
          const pop = parseInt(popRaw.replaceAll(',',''), 10)
          lines.push(`("${stateRaw}", ${pop}), `)
        }
      })
    }
  })
  console.log(lines.join('\n'))
})()
