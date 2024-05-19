/**
 * Load data from the url set as link in the top 3 cells in the active sheet:
 *  Clears the content (+ formatting) from the 2nd row
 *
 *  loads data from all 3 urls using {{loadData}}
 *  Sorts the whole sheet by the scala version, benchmark type then collection size 64 (columns 1,2,8)
 */
function Load_Data() {
  var dataRange = getDataRange()
  let sheet = dataRange.getSheet()
  let filter = sheet.getFilter();
  if (filter) {
    filter.remove();
  }
  if(dataRange.getNumRows()>1) {
    dataRange.clear()
  }
  ['2.12','2.13','3.3'].forEach((ver, i) => {
    var url = sheet.getRange(1, i+1).getRichTextValue().getLinkUrl()
    loadData(sheet, ver, url)
  })

  let range = getDataRange()
  range.sort([{column: 1, ascending: true}, {column: 2, ascending: true}, {column: 8, ascending: true}])
  Add_Formatting();
  sheet.autoResizeColumns(1, range.getNumColumns())
}

/** Adds conditional formatting groupped by the second column
  * Also adds ratio in each row base on the first row in the section. => The data expected to be sorted in ascending order
  * If the actual ratio is less than 2 => sets the difference as percentage
  * The multiplier otherwise
  * Adds conditional formatting for the range groupped by the 2nd column (benchmark type) using [[condFormat]]
  */
function Add_Formatting() {
  let allContent = getDataRange()
  let sheet = allContent.getSheet()
  allContent.clearFormat()
  var benchStartRow = 2
  for(var row = benchStartRow; row < allContent.getNumRows();) {
    let refRow = sheet.getRange(row, 2, 1, allContent.getNumColumns()).getValues()[0]
    let caze = refRow[0]
    while (caze == sheet.getRange(++row, 2).getValue()) {
      col = 4
      let vals = sheet.getRange(row, col, 1, allContent.getNumColumns()-col+2).getValues()[0]
      for(; refRow[col-2] > 0; col += 2 ) {
        //sheet.getRange(row, col).activate()
        var v = vals[col-4]
        if(v > 0) {
          var pct = (v / refRow[col-2])
          var fmt = '0.0 x'
          if(pct < 2) {
            fmt = '0%'
            pct -= 1
          } else if (pct > 10) {
            fmt = '0 x'
          }
          sheet.getRange(row, col+1).setValue(pct).setNumberFormat(fmt);
        }
      }
    }
    for(var col = allContent.getNumColumns(); col > 3; col--){
      condFormat(sheet.getRange(benchStartRow, col, row-benchStartRow,1))
    }
    sheet.getRange(benchStartRow,2,1,allContent.getNumColumns()-1).setBorder(true, null, null, null, false, false)
    benchStartRow = row
  }
}

function getDataRange() {
  let spreadsheet = SpreadsheetApp.getActive();
  let sheet = spreadsheet.getActiveSheet()
  let allContent = sheet.getDataRange()
  if(allContent.getNumRows() > 1) {
    return sheet.getRange(2, 1, allContent.getNumRows()-1, allContent.getNumColumns())
  }
  return allContent
}

/** Loads data from {url} into {sheet}
 * Fills the 1st row from cell 3 with the sizes
 * Creates rows [ver], {benchmark}, {case}, {times... skipping 1 column between}
*/
function loadData(sheet, ver, url) {
  let response = UrlFetchApp.fetch(url);
  let data = JSON.parse(response.getContentText());

  var col = 4
  for(size of data.sizes) {
    let range = sheet.getRange(1, col, 1, 2)
    range.setValue(size)
    range.merge()
    col += 2
  }
  let allContent = sheet.getDataRange()
  var row = allContent.getNumRows()
  for(let benchName in data.result) {
    var bench = data.result[benchName]
    for(let caseName in bench) {
      let times = bench[caseName]
      var col = 1; row++
      sheet.getRange(row, col++).setValue(ver)
      sheet.getRange(row, col++).setValue(benchName)
      sheet.getRange(row, col++).setValue(caseName)

      for(let time of times) {
        sheet.getRange(row, col).setValue(time)
        col += 2
      }
    }

  }
  //return data;
}

function condFormat(range) {
  var conditionalFormatRules = range.getSheet().getConditionalFormatRules();
  conditionalFormatRules.push(SpreadsheetApp.newConditionalFormatRule()
    .setRanges([range])
    .setGradientMinpoint('#57BB8A')
    .setGradientMidpointWithValue('#FFFFFF', SpreadsheetApp.InterpolationType.PERCENTILE, '50')
    .setGradientMaxpoint('#E67C73')
    .build());
  range.getSheet().setConditionalFormatRules(conditionalFormatRules);
}
