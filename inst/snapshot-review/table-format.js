// JASP Desktop's table formatter, distributed under AGPL-3.0-or-later.
// Source: https://github.com/jasp-stats/jasp-desktop/blob/3302d1ab81bb5478ff0b2e8809a73b8df8e5d280/Desktop/html/js/utils.js
// Refresh the locale variables and functions formatMoney through formatColumn,
// fSD, fSDOE, getDecimalSeparator and toExponential from the same revision.
// See JASP-Desktop-LICENSE.txt or https://www.gnu.org/licenses/agpl-3.0.html.

window.snapshotTableDisplay = (() => {
  // Defaults in Desktop/html/js/main.js and Desktop/utilities/settings.cpp.
  const window = {globSet: {pExact: false, decimals: "", normalizedNotation: true}};
  let _currency; // The upstream formatMoney call assigns this variable.

  // Begin Desktop formatter
var currentLocaleId = "en"
var useThousandsSeparators = true

function formatMoney(_currency='EUR', amount, thousands=true) {
	const formatter = new Intl.NumberFormat(currentLocaleId, {
		style: 'currency',
		currency: _currency,
		trailingZeroDisplay: 'stripIfInteger',
		useGrouping: useThousandsSeparators || thousands
	});

	return amount == "." ? amount : formatter.format(amount)
}

function handleNoZeroLead(number, result, noZeroLead)
{
	if(number >= 0 && number < 1 && noZeroLead && !result.startsWith("1")) //we need to check whether it starts with 1 despite number < 1 because of rounding in the formatting
		return result.substring(1);
	return result;
}

function formatFixed(number, digitsFrac, noZeroLead=false, thousands=false) {
	if(isNaN(digitsFrac))
		digitsFrac = 0
	const formatter = new Intl.NumberFormat(currentLocaleId, { minimumFractionDigits: digitsFrac, maximumFractionDigits: digitsFrac, useGrouping: thousands});

	var result = formatter.format(number)

	return handleNoZeroLead(number, result, noZeroLead);
}

function formatPrecision(number, precision, noZeroLead=false, thousands=false) {
	const formatter = new Intl.NumberFormat(currentLocaleId, { minimumSignificantDigits: precision, maximumSignificantDigits: precision, useGrouping: thousands });

	var result = formatter.format(number)

	return handleNoZeroLead(number, result, noZeroLead);
}

function formatPrecisionWithRespectForFixedDecimals(number, sf, dp, noZeroLead=false, thousands=false) {
	if(isNaN(dp))
		return formatPrecision(number, sf, noZeroLead);

	const formatter = new Intl.NumberFormat(currentLocaleId, {
		minimumSignificantDigits:	sf, maximumSignificantDigits:	sf,
		minimumFractionDigits:		dp, maximumFractionDigits:		dp,
		roundingPriority:			"lessPrecision",
		useGrouping:				thousands
	});

	var result = formatter.format(number)

	return handleNoZeroLead(number, result, noZeroLead);
}

function formatNumber(number, thousands=false) {
	const formatter = new Intl.NumberFormat(currentLocaleId, { useGrouping: thousands });

	return formatter.format(number)
}

function formatColumn(column, type, format, alignNumbers, combine, modelFootnotes, html = true, errorOnMixed = false) {
	/**
	 * Prepares the columns of a table to the required format
	 * @param column
	 * @param type           column type - string, number, pvalue, integer, ...
	 * @param format         decimal format, p-value format
	 * @param alignNumbers
	 * @param combine
	 * @param modelFootnotes
	 * @param html           html render or latex code. Default is true
	 * @param errorOnMixed   internal, used to avoid infinite loops
	 */

	let columnCells = Array(column.length);

	if (type === "mixed") {

		if (errorOnMixed) {
			// or throw an error? But how?
			return "Error: nested mixed columns are not supported!";
		}

		// call formatColumn for each row of the mixed column
		for (let rowNo = 0; rowNo < column.length; rowNo++) {
			// we need to construct the array [column[rowNo].content.value] because column.length otherwise does not exist.
			columnCells[rowNo] = formatColumn([{content: column[rowNo].content.value}], column[rowNo].content.type, column[rowNo].content.format, alignNumbers, combine, modelFootnotes, html, true)[0];
		}

		return columnCells;

	}

	if (type === "string" || typeof format == "undefined" || format === null) {

		for (let rowNo = 0; rowNo < column.length; rowNo++) {

			let clazz		= (type === "string" && !errorOnMixed) ? "text" : "number";
			let cell		= column[rowNo];
			let content		= cell.content;
			let contentNum	= parseFloat(content)
			let isNumber	= !isNaN(contentNum) && clazz == "number"
			let formatted	= { content: (isNumber ? formatNumber(contentNum) : content) }
			let combined	= false;

			formatted["class"] = isNumber ? "number" : clazz

			if (typeof content == "undefined") {
				formatted["content"] = "."

			} else if (combine && rowNo > 0 && column[rowNo - 1].content == content) {
				formatted["class"]	+= " combined";
				formatted["content"] = html ? "&nbsp;" : " "
				combined = true;
			} else if(!isNumber) {
				if (typeof content === "string" && html)
					content = content.replace(/\u273B/g, "<small>\u273B</small>");

				formatted["content"] = content
			}

			if (combined == false && cell.isStartOfGroup)
				formatted.isStartOfGroup = true;

			if (cell.isStartOfSubGroup)
				formatted.isStartOfSubGroup = true;

			if (cell.isEndOfGroup)
				formatted.isEndOfGroup = true;

			if (typeof cell.footnotes != "undefined")
				formatted.footnotes = getFootnotes(modelFootnotes, cell.footnotes);

			columnCells[rowNo] = formatted;
		}
		return columnCells
	}

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////			First collect the formats and their respective settings        ////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	let formats		= format.split(";");
	let p			= NaN;
	let isP			= false;
	let dp			= parseInt(window.globSet.decimals);
	let sf			= NaN;
	let pc			= false;
	let approx		= false;
	let fixDecimals = typeof dp === 'number' && dp >= 0;
	let currency	= ""
	let moneyFmt	= "monetary"
	let noZeroLead	= false;
	let thousands	= false

	for (let i = 0; i < formats.length; i++)
	{
		let f = formats[i];
		if (f.match(/^p:/) !== null) {
			// override APA style if exact p-values wanted
			if (window.globSet.pExact) {
				sf = 4;
			} else {
				p = fixDecimals ? 1/Math.pow(10,dp) : Number(f.substring(2));
				noZeroLead = true;
			}

			isP = true;
		}

		if(f.startsWith("thousands"))
			thousands = true

		if(f.startsWith(moneyFmt))
		{
			if(f.length > moneyFmt.length)
			{
				currency = f.substr(moneyFmt.length)
				if(currency.startsWith(":"))
					currency = currency.substring(1)
			}

			if(currency == "")
				currency = "EUR"
		}

		if (f.indexOf("dp:") != -1 && !fixDecimals)
			dp = Number(f.substring(3));

		if (f.indexOf("sf:") != -1)
			sf = Number(f.substring(3));

		if (f.indexOf("pc") != -1 || f.indexOf("percentage") != -1)
		{
			pc = true;

			if(!fixDecimals)
				dp = 2

			let colonPos 	= f.indexOf(":")

			if(colonPos != -1)
			{
				let pcDP = f.substr(colonPos + 1);

				if(!isNaN(parseFloat(pcDP)))
					dp = parseFloat(pcDP)
			}
		}

		if (f.indexOf("~") != -1)
			approx = true;
	}

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////	  Then try to convert all the cells to float if they werent numbers    ////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	for (var rowNo = 0; rowNo < column.length; rowNo++) {
		let cell = column[rowNo]
		let content = cell.content

		if (typeof(content) !== "number" || typeof(content) !== "Number") {
			if (isNaN(parseFloat(content)))  // isn't a number
				continue
			cell.content = content = parseFloat(content)
		}
	}

	// some vars we need for sf:
	var upperLimit	= 1e6
	var minLSD		= Infinity	// right most position of the least significant digit
	var maxFSDOE	= -Infinity  // left most position of the least significant digit of the exponent in scientific notation

	if (isFinite(sf)) {
		//prepare some stuff we need to run "sf"

		for (var rowNo = 0; rowNo < column.length; rowNo++)
		{
			let cell 	= column[rowNo]
			let content = cell.content

			if (isNaN(parseFloat(content)))  // isn't a number
				continue

			let fsd =fSD(content) // position of first significant digit
			let lsd = fsd - sf
			let fsdoe

			if (Math.abs(content) >= upperLimit || Math.abs(content) <= Math.pow(10, -dp))
				fsdoe = fSDOE(content)   // first significant digit of exponent

			if (fsdoe > maxFSDOE)
				maxFSDOE = fsdoe

			if (lsd < minLSD)
				minLSD = lsd
		}

		var _minDp = isFinite(dp) ? -dp : -Infinity;
		minLSD = fixDecimals ? -dp : Math.min(0, Math.max(_minDp, minLSD))
		minLSD = Math.max(-20, minLSD)
	}

	format = currency != "" ? "monetary" : pc ? "percentage" : isFinite(sf) ? "significance" : isFinite(dp) ? "decimalPoints" : "other"

	//Now that thats been determined we can format our cells
	for (var rowNo = 0; rowNo < column.length; rowNo++)
	{
		//Get the cell and set up content var everything will read
		//and the formatted var for writing to the new cells at the end.
		let cell 		= column[rowNo]
		let content 	= cell.content
		let formatted	= { content: content, class: ""}
		let isNumber	= format == "significance" || format == "decimalPoints" || format == "percentage" //Will set class number
		let isPercent	= format == "percentage"		// will set class percentage

		if (typeof content == "undefined")
			formatted["content"] = "."
		else if (content === "" || (combine && rowNo > 0 && column[rowNo - 1].content == content))
			formatted["content"] = html ? "&nbsp;" : " "
		else
			switch(format)
			{
				case "other": //Lets try to format it as a number I guess
				{
					let aNumberPerhaps = parseFloat(content)

					if(!isNaN(aNumberPerhaps))
					{
						formatted["content"] = formatNumber(aNumberPerhaps, thousands)
						isNumber = true
					}

					break;
				}
				case "monetary":
				{
					formatted["content"] = formatMoney(_currency=currency, content, thousands)
					formatted["class"]	 = "monetary"
					break;
				}

				case "percentage":
				{
					if (!isNaN(parseFloat(content)))
						formatted["content"] = "" + (formatFixed(content * 100, dp, false, thousands)) + (html ? "&thinsp;%" : "%")
					break;
				}

				case "significance":
				{
					if (isNaN(parseFloat(content)))  // isn't a number but we'll still give it that class
					{
						formatted["class"] = "number"
						isNumber = false
					}
					else if (content < p)
					{
						formatted["content"] 	= (html ? "<&nbsp;" : "< ") + formatPrecisionWithRespectForFixedDecimals(p, sf, noZeroLead, thousands)
						formatted["class"]		= "p-value"
						isNumber = false
					}
					else if (content == 0)
					{
						formatted["content"] = (fixDecimals || isFinite(dp)) ? formatFixed(content, dp, noZeroLead, thousands) : formatPrecision(content, sf, false, thousands)
					}
					else if (Math.abs(content) >= upperLimit || Math.abs(content) < Math.pow(10, -dp) || (!fixDecimals && Math.abs(content) >= Math.pow(10, sf)))
					{
						let decimalsExpon 		= fixDecimals ? dp : sf - 1;
						// var paddingNeeded = Math.max(maxFSDOE - fSDOE(content), 0)
						var paddingNeeded = 0
						formatted["content"] 	= toExponential(content, decimalsExpon, paddingNeeded, html)
					}
					else
					{
						if(isP)						formatted["content"] = fixDecimals || window.globSet.pExact ? toExponential(content, (isFinite(dp) ? dp : sf-1), 0, html)	: formatPrecisionWithRespectForFixedDecimals(content, sf, dp, noZeroLead, thousands)
						else						formatted["content"] = fixDecimals ? formatFixed(content, -minLSD, false, thousands) : formatPrecision(content, sf, false, thousands)

						if(html)
							formatted["content"] = formatted["content"].replace(/-/g, "&minus;")
					}

					break;
				}

				case "decimalPoints":
				{
					if (isNaN(parseFloat(content)))  // isn't a number but we'll give it that class anyway
					{
						formatted["class"] = "number"
						isNumber = false
					}
					else if (content < p)
					{
						formatted["content"] 	= (html ? "<&nbsp;" : "< ") + formatFixed(p, dp, noZeroLead, thousands)
						formatted["class"]		= "p-value"
						isNumber = false
					}
					else
					{
						var strContent = "";

						if (p && content != 0 && Math.abs(content) < 1/(Math.pow(10,dp)))
							strContent = toExponential(content, dp, 0, html)
						else
						{
							strContent = formatFixed(content, dp, noZeroLead, thousands)
							if(html)
								strContent = strContent.replace(/-/g, "&minus;")
						}
						formatted["content"] = strContent
					}
					break;
				}

			}

		if(isNumber)
			formatted["class"] = "number"

		if(isPercent)
			formatted["class"] = "percentage"

		if (isNumber && approx)
			formatted.content = (html ? "~&thinsp;" : "~") + formatted.content

		//Finishing up:
		if (typeof cell.footnotes != "undefined")
			formatted["footnotes"] = getFootnotes(modelFootnotes, cell.footnotes)

		if (cell.isStartOfGroup)
			formatted["class"] += " new-group-row"

		if (cell.isStartOfSubGroup)
			formatted["class"] += " new-sub-group-row"

		if (cell.isEndOfGroup)
			formatted["class"] += " last-group-row"

		//And at last assign the formatted string to its cell:
		columnCells[rowNo] = formatted
	}

	return columnCells

}

function fSD(value) {
	/**
	 * First significant digit position
	 * @param value
	 */

	if (value > 0)
		return Math.floor(Math.log(+value) / Math.log(10)) + 1
	else if (value < 0)
		return Math.floor(Math.log(-value) / Math.log(10)) + 1
	else
		return 1

}

function fSDOE(value) {
	/**
	 * First significant digit position in the exponent in scientific notation
	 * @param value
	 */

	if (value == 0)
		return 1

	var exponent = Math.floor(Math.log(Math.abs(value)) / Math.log(10))

	return fSD(exponent)

}

function getDecimalSeparator()
{
    const numberWithDecimalSeparator = 1.1;

    return (new Intl.NumberFormat(currentLocaleId))
		.formatToParts(numberWithDecimalSeparator)
        .find(part => part.type === 'decimal')
        .value;
}

function toExponential(number, decimalsExpon, paddingNeeded, html) {
	let _sign			= (html) ? "&minus;" : "-";
	let exponentiated	= number.toExponential(decimalsExpon).replace(/-/g, _sign).replace(".", getDecimalSeparator())
	let split			= exponentiated.split("e")
	let mantissa		= split[0]
	let exponent		= split[1]
	let padding			= !paddingNeeded 	? ''	: '<span class="do-not-copy" style="visibility: hidden;">' + Array(paddingNeeded + 1).join("0") + '</span>'

	let reassembled  = mantissa;
		reassembled += !window.globSet.normalizedNotation ? "e" : (html ? "&times;10" : "×10") + "<sup>"
		reassembled += html ? padding : ""
		reassembled += exponent
		reassembled += !window.globSet.normalizedNotation ? "" : "</sup>"


	return reassembled;
}
  // End Desktop formatter

  // Keep the visible exponent in plain text so the reviewer can compare digits
  // and highlight them without inserting snapshot markup in the document.
  const digits = "0123456789+-−", superscripts = "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁻";
  function cellText(content, template) {
    const text = content === undefined ? "" : String(content);
    if (!/[<&]/.test(text)) return text;
    template.innerHTML = text;
    for (const hidden of template.content.querySelectorAll(".do-not-copy, script, style"))
      hidden.remove();
    for (const sup of template.content.querySelectorAll("sup"))
      sup.textContent = sup.textContent.replace(/[0-9+−-]/g, digit => superscripts[digits.indexOf(digit)]);
    return template.content.textContent;
  }

  return function snapshotTableDisplay(table) {
    const rows = table?.data ?? [];
    const displayed = rows.map(() => Object.create(null));
    const template = document.createElement("template");
    for (const [index, field] of (table?.schema?.fields ?? []).entries()) {
      // Match createColumns() before passing each entire column to formatColumn().
      // The review attaches footnotes to the resulting text separately.
      const column = rows.map(row => {
        let content = Object.hasOwn(row, field.name) ? row[field.name] ?? "" : "";
        if (index === 0 && field.type === "string" && row[".rowLevel"])
          content = "&nbsp;".repeat(5 * row[".rowLevel"]) + content;
        return {content};
      });
      const cells = formatColumn(column, field.type, field.format, !table.casesAcrossColumns, field.combine, [], true);
      cells.forEach((cell, i) => { displayed[i][field.name] = cellText(cell.content, template); });
    }
    return displayed;
  };
})();
