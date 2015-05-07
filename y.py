import xlrd

workbook = xlrd.open_workbook("gartner_hype.xls")
worksheet = workbook.sheet_by_index(0)
sheet.cell(0,0).value
