bin_PROGRAMS = status-report status-report-server
status_report_SOURCES = client.scm
status_report_server_SOURCES = server.scm

.scm.o:
OBJECT=o
LINK=csc -o $@

%.o: %.scm
	csc -c $< -o $@ $(CHICKENFLAGS)
