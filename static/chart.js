$(document).ready(function() {
  var NUM_TASKS = 60;

  function renderChart(floor_list, count_list) {
    var chart = new Highcharts.Chart({
      chart: {
        renderTo: 'chart',
        type: 'column'
      },
      legend: {
        enabled: false
      },
      title: {
        text: 'Scavenger Hunt'
      },
      xAxis: {
        categories: floor_list
      },
      yAxis: {
        max: NUM_TASKS,
        title: {
          text: "Completed tasks"
        }
      },
      tooltip: {
        formatter: function() {
          var percent = ((this.y / NUM_TASKS) * 100).toFixed(1) + "%";
          return '' + this.y + " (" + percent + ")";
        }
      },
      series: [{
        name: 'Completed tasks',
        data: count_list
      }]
    });

    return chart;
  }

  $.getJSON('/update', function(data) {
    var data = data["data"];
    console.log(data);
    var floor_list = [];

    for (floor in data) {
      if (data.hasOwnProperty(floor)) {
        floor_list.push(floor);
      }
    }
    floor_list.sort();

    // Create a count list in accordance to ordering of floor_list
    var count_list = []
    for (var i = 0; i < floor_list.length; i += 1) {
      count_list.push(data[floor_list[i]].length);
    }

    var chart = renderChart(floor_list, count_list);
  });
});

