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

  function populate_rankings(floor_list, data) {
    var create_li = function (ranked_list, index) {
      var $li = $("<li><a href='#'>" + ranked_list[index] + "</a></li>");
      $li.click(function(e) {
        $(".tasks-list").hide();
        $("#team-" + index).show();

        e.preventDefault();
      });

      return $li;
    };

    var ranked_list = floor_list.slice();
    ranked_list.sort(function(a, b) {
      return data[b].length - data[a].length;
    });

    var $rankings = $("#rankings");
    var $tasks = $("#tasks");

    $("#rankings-container").show();
    for (var i = 0; i < ranked_list.length; i += 1) {
      $rankings.append(create_li(ranked_list, i));

      var $div = $("<div id='team-" + i + "' class='tasks-list'></div>");
      $div.append("<h2>" + ranked_list[i] + "'s completed tasks</h2>");

      var task_list = data[ranked_list[i]]
      var $task_display;
      if (task_list.length > 0) {
        var $task_display = $("<ul></ul>")
        for (var j = 0; j < task_list.length; j += 1) {
          $task_display.append("<li>" + task_list[j] + "</li>");
        }
      } else {
        $task_display = $("<p>There's nothing here :(</p>");
      }
      $div.append($task_display);
      $tasks.append($div);
    }

    $(".tasks-list").hide();
  }

  $.getJSON('/update', function(data) {
    var data = data["data"];
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

    populate_rankings(floor_list, data);
  });
});

