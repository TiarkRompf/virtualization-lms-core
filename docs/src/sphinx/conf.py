# -*- coding: utf-8 -*-

import sys, os

sys.path.append(os.path.abspath('_sphinx/exts'))
extensions = ['sphinx.ext.extlinks', 'howto', 'includecode']
# Project variables

project = 'lms'
version = '0.2'
release = '0.2'

# General settings

needs_sphinx = '1.1'
nitpicky = True
default_role = 'literal'
master_doc = 'index'
highlight_language = 'scala'
add_function_parentheses = False

# HTML

html_theme = 'lms'
html_theme_path = ['_sphinx/themes']
html_title = 'LMS Documentation'
html_domain_indices = False
html_use_index = False
html_show_sphinx = False
html_use_smartypants = False

# if true:
#  the Home link is to scala-sbt.org
# if false:
#  the Home link is to home.html for the current documentation version
# TODO: pass this as an argument to sphinx
home_site = True

# Passed to Google as site:<site_search_base>
# If empty, no search box is included
# TODO: pass this as an argument to sphinx, use actual version instead of release
site_search_base = 'http://www.scala-sbt.org/release/docs'

# passes variables to the template
html_context = {'home_site': home_site, 'site_search_base': site_search_base}

# Latex (PDF)

#latex_documents = [
#  ('pdf_index', 'sbt.tex', html_title, '', 'manual', True),
#]

# Issues role

issuetracker = 'github'
issuetracker_project = 'TiarkRompf/virtualization-lms-core'
issuetracker_plaintext_issues = True
issuetracker_issue_pattern = r'\bgh-(\d+)\b'
issuetracker_title_template = '#{issue.id}'

# links, substitutions
rst_epilog = """
.. _source code: http://github.com/TiarkRompf/virtualization-lms-core
""" % {
}
