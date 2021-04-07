var mysql = require('mysql'); 
var con = mysql.createConnection({
    host: "localhost",
    user: "dwapi",
    password: "",
    database:"portaldev"
  });
  
  var sql ='Select * from arimahts';
  con.connect(function(err) {
    if (err) throw err;
    con.query(sql, function (err, result, fields) {
      if (err) throw err;
      console.log(result);
    });
  });