
var publicSpreadsheetUrl = 'https://docs.google.com/spreadsheets/d/104eRCUeyIsyHZpWiGP5XcZ4oiXnBBos55Z5uUYh0TpM/pubhtml';

function renderSpreadsheetData() {
	Tabletop.init( { key: publicSpreadsheetUrl,
				 callback: draw,
				 simpleSheet: true } )
}

function draw(data, tabletop) {

	results = tabletop.sheets("Form Responses 1")
	main_data = results.elements

	var denom = main_data.length

	var fruits = ['Raspberries',
				'Strawberries',
				'Bananas',
				'Watermelon',
				'Green Apples',
				'Blueberries',
				'Canteloupe',
				'Honeydew',
				'Kiwi',
				'Mango',
				'Apricots',
				'Blackberries',
				'Clementines',
				'Cherries',
				'Grapes',
				'Oranges',
				'Peaches',
				'Pears',
				'Pineapple',
				'Grapefruit',
				'Red Apples']

	var randomItem = fruits[Math.floor(Math.random()*fruits.length)];	

	console.log(randomItem)		

	var_select = 'Assign the following fruits to tiers (where A-tier is the best/highest quality and F-tier is reserved for the fruits that deserve to be banished).  [' + randomItem + ']'

	 all = _.map(_.groupBy(main_data,var_select), function (item, key) {
	    var size = item.length,
	        obj={};
	    var positiveCount = _.countBy(item, function (i) {
	        return i.answer == "A-tier" ? "A-tier" : "count";
		});
	    console.log(positiveCount)
	    var percentagePossitive = (positiveCount.count / denom) * 100;

    	obj['key'] = key
    	obj['value'] = percentagePossitive || 0;
    	return obj;
		});

	console.log(all)

	var order = ["A-tier", "B-tier", "C-tier", "D-tier","F-tier","Don't Know/Care"];
	all = _.sortBy(all, function(obj){ 
	    return _.indexOf(order, obj.key);
	});

	// set the dimensions and margins of the graph
	var margin = {top: 20, right: 20, bottom: 30, left: 40},
		width = 960 - margin.left - margin.right,
		height = 500 - margin.top - margin.bottom;

	// set the ranges
	var x = d3.scaleBand()
	      .range([0, width])
	      .padding(0.1);
	var y = d3.scaleLinear()
	      .range([height, 0]);
	      
	// append the svg object to the body of the page
	// append a 'group' element to 'svg'
	// moves the 'group' element to the top left margin
	var svg = d3.select("body").append("svg")
	.attr("width", width + margin.left + margin.right)
	.attr("height", height + margin.top + margin.bottom)
	.append("g")
	.attr("transform", 
	      "translate(" + margin.left + "," + margin.top + ")");

	// Scale the range of the data in the domains
	x.domain(all.map(function(d) { return d.key; }));
	y.domain([0, d3.max(all, function(d) { return d.value; })]);

	// append the rectangles for the bar chart
	svg.selectAll(".bar")
	  .data(all)
	.enter().append("rect")
	  .attr("class", "bar")
	  .attr("x", function(d) { return x(d.key); })
	  .attr("width", x.bandwidth())
	  .attr("y", function(d) { return y(d.value); })
	  .attr("height", function(d) { return height - y(d.value); });

	// add the x Axis
	svg.append("g")
	  .attr("transform", "translate(0," + height + ")")
	  .call(d3.axisBottom(x));

	// add the y Axis
	svg.append("g")
	  .call(d3.axisLeft(y));

}

renderSpreadsheetData();
