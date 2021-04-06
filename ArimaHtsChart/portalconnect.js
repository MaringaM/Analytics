const mysql = require('mysql'); 
const path = require('path');
const express = require('express')
const app = express()
const port = 3000

app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname + '/index.htm'));
})

app.get('/arima', (req, res) => {
  var con = mysql.createConnection({
    host: "localhost",
    user: "dwapi",
    password: "",
    database:"portaldev"
  });
  
  var sql ="Select distinct date_format(date,'%Y-%m-%d') as date,num_pos,Point_Forecast,max(lo_95) as lo_95,max(hi_95) as hi_95 from arimahts where county='bungoma'group by date_format(date,'%Y-%m-%d'),num_pos,Point_Forecast";
  con.connect(function(err) {
    if (err) throw err;
    con.query(sql, function (err, result, fields) {
      if (err) throw err;
      res.send(result)
    });
  });
  
})

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})