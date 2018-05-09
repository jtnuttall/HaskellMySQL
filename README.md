# HaskellMySQL

This is program takes book and author JSON dumps found on the Open Library website
and inserts them into a MySQL relational database.

It is extremely memory-efficient, as it streams only one JSON object at a time
from the file, which is important given the sheer size of the files.