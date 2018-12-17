Nameres database
----------------

This folder contains a set of tools used to track name resolution analytics:

- `import_data.py` is a script that takes as input a file containing JSON data
  as dumped by Libadalang's `nameres` executable, and fills a SQLite database
  with it.

- `dashboard.py` is a small web application that will present statistics
  computed on top of the resulting database. You can just run `python
  dashboard.py` and you'll have a web server on `localhost:8000`.

- `schema.py` is a specification of the database schema, along with some
  functions computing important stats. If you wish to compute stats that are
  not computed/shown on the dashboard, you can import it in a ipython terminal,
  or directly run SQL queries against the database.
