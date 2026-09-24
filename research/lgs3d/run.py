#!/usr/bin/env python3
"""Portable adapter launcher. Child watchdog starts before importing NumPy."""
import os
import sys
import threading
import time

if len(sys.argv)>1 and sys.argv[1]=='--worker':
    owner=int(sys.argv[4])
    def watch_owner():
        while True:
            if os.getppid()!=owner:
                os._exit(125)
            time.sleep(.05)
    threading.Thread(target=watch_owner,daemon=True).start()
    from lgs_paper.adapter import worker
    worker(sys.argv[2],sys.argv[3])
else:
    from lgs_paper.adapter import main
    raise SystemExit(main())
