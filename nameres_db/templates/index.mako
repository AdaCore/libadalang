<%def name="donut(title, id, data)">
<canvas id="${ id }-donut"></canvas>
<script>
(function () {
  var colors = ["rgb(255, 99, 132)","rgb(54, 162, 235)","rgb(255, 205, 86)"];
  var data = {
      labels: ${json.dumps(data[0])},
      datasets: [
        {
          backgroundColor: colors,
          data: ${json.dumps(data[1])}
        }
      ]
  };
  var donut = document.getElementById("${id}-donut");
  if (donut) {
    new Chart(donut, {
        type: 'pie',
        data: data,
        options: { cutoutPercentage: 50 }
    });
  }
})();
</script>
</%def>

<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <meta name="author" content="">
    <link rel="icon" href="../../../../favicon.ico">

    <title>Dashboard Template for Bootstrap</title>

    <!-- Bootstrap core CSS -->
  <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">

    <!-- Custom styles for this template -->
    <link href="${ url_for("static", filename="dashboard.css")}" rel="stylesheet">
  </head>

  <body>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.7.1/Chart.min.js"></script>
    <nav class="navbar navbar-dark fixed-top bg-dark flex-md-nowrap p-0 shadow">
      <a class="navbar-brand col-sm-3 col-md-2 mr-0" href="#">Name resolution dashboard</a>
      <input class="form-control form-control-dark w-100" type="text" placeholder="Search" aria-label="Search">
      <ul class="navbar-nav px-3">
        <li class="nav-item text-nowrap">
          <a class="nav-link" href="#">Sign out</a>
        </li>
      </ul>
    </nav>

    <div class="container-fluid">
      <div class="row">
        <nav class="col-md-2 d-none d-md-block bg-light sidebar">
          <div class="sidebar-sticky">
            <ul class="nav flex-column">
              <li class="nav-item">
                <a class="nav-link active" href="#">
                  <span data-feather="home"></span>
                  Dashboard <span class="sr-only">(current)</span>
                </a>
              </li>

            <h6 class="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
              <span>Projects</span>
            </h6>

              % for project in projects:
              <li class="nav-item">
                <a class="nav-link" href="#">
                  <span data-feather="file"></span>
                  ${ project.name }
                </a>
              </li>
              % endfor
            </ul>

          </div>
        </nav>

        <main role="main" class="col-md-9 ml-sm-auto col-lg-10 px-4">
          <div class="d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom">
            <h1 class="h2">Main dashboard</h1>
          </div>
          <div class="row my-3">
              <div class="col">
                  <h4>Global failures dashboard</h4>
              </div>
          </div>

          <div class="row">

              <div class="col">
                  <div class="card">
                      <div class="card-body">
                        <h5 class="card-title"> Node resolutions for all projects </h5>
                        <% 
                            fbt = S.failures_by_traceback()
                            data = [
                                [range(len(fbt))],
                                []
                            ]
                        %>
                        ${donut("Failures by traceback", "failures-bytb", data)}
                        <canvas id="all-projects-donut"></canvas>
                      </div>
                  </div>
              </div>

              <div class="col">
                  <div class="card">
                      <div class="card-body">
                        <h5 class="card-title"> Node resolutions for all projects </h5>
                        <% data = [
                            ['Successes', 'Failures'],
                            [stats['nb_successes'], stats['nb_failures']]
                        ] %>
                        ${donut("Name resolution for all projects", "all-projects", data)}
                        <canvas id="all-projects-donut"></canvas>
                      </div>
                  </div>
              </div>
          </div>

          % for project_group in F.chunks(3, projects):
          <div class="row py-2">
              % for project in project_group:
              <div class="col-md-4 py-1">
                  <div class="card">
                      <div class="card-body">
                        <h5 class="card-title"> Node resolutions for ${ project.name } </h5>
                        <% data = [
                            ['Successes', 'Failures'],
                            [project.nb_successes, project.nb_failures]
                        ] %>
                        ${donut("Name resolution for project " + project.name, project.name, data)}

                        <a href="#" class="btn btn-primary">Project page</a>
                      </div>
                  </div>
              </div>
              % endfor
          </div>
          % endfor
        </main>
      </div>
    </div>

    <!-- Bootstrap core JavaScript
    ================================================== -->
    <!-- Placed at the end of the document so the pages load faster -->

  <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" crossorigin="anonymous"></script>
  <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" integrity="sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy" crossorigin="anonymous"></script>
    <!-- Icons -->
    <script src="https://unpkg.com/feather-icons/dist/feather.min.js"></script>
    <script>
      feather.replace()
    </script>

    <!-- Graphs -->
  </body>
</html>
