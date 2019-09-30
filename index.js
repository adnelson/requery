const { renderQuery } = require('./src/SqlQuery.bs')
const { mainQuery } = require('./src/Examples.bs')
console.log(renderQuery(mainQuery))

process.exit()
