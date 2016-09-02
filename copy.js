const inputDir = process.argv[2]
const inputPath = process.argv[3];
const outputDir = process.argv[4];

const fs = require('fs');
// const tree = JSON.parse(fs.readFileSync(inputPath));

const each = (coll, f) => Array.isArray(coll) ? coll.forEach(f) : Object.keys(coll).map((k) => f(coll[k], k, coll));

function copyFile(source, target, cb) {
  var cbCalled = false;

  var rd = fs.createReadStream(source);
  rd.on("error", function(err) {
    done(err);
  });
  var wr = fs.createWriteStream(target);
  wr.on("error", function(err) {
    done(err);
  });
  wr.on("close", function(ex) {
    done();
  });
  rd.pipe(wr);

  function done(err) {
    if (!cbCalled) {
      cb(err);
      cbCalled = true;
    }
  }
}

const tree = {"withoutCamera":
 ["f14208720", "f14221480", "f14227936", "f14232456", "f14238696",
  "f14249600", "f14259160", "f14266712"],
 "withCamera":
 {"withDate":
  {"2014":
   {"7":["f17010128", "f17017976", "f17040936", "f17205232"],
    "8":["f17133672", "f17182776"]},
   "2015":{"2":["f17378600", "f17378600"]}}}};

const save = (tree, path) => {
  if ('string' === typeof tree) {
    
  }
}
