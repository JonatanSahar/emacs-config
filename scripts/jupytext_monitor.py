#!/usr/bin/env python3
"""
File monitor daemon that runs jupytext on changed Python and Jupyter notebook files.
"""

import os
import sys
import time
import argparse
import subprocess
import logging
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

class JupytextHandler(FileSystemEventHandler):
    """Handler for file system events that runs jupytext on changed files."""
    
    def __init__(self, patterns=None):
        self.patterns = patterns or ['.py', '.ipynb']
        self.last_processed = {}  # Track last processed time to avoid duplicates
        
    def on_modified(self, event):
        if event.is_directory:
            return
            
        file_path = event.src_path
        file_ext = os.path.splitext(file_path)[1]
        
        # Check if the file has a monitored extension
        if file_ext not in self.patterns:
            return
            
        # Avoid processing the same file multiple times in quick succession
        current_time = time.time()
        if file_path in self.last_processed:
            if current_time - self.last_processed[file_path] < 1:  # 1 second debounce
                return
                
        self.last_processed[file_path] = current_time
        
        # Run jupytext on the file
        logger.info(f"File changed: {file_path}")
        try:
            cmd = ["jupytext", "--set-formats", "py:percent,ipynb", file_path]
            logger.info(f"Running: {' '.join(cmd)}")
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            if result.returncode == 0:
                logger.info(f"Successfully synced {file_path}")
            else:
                logger.error(f"Error syncing {file_path}: {result.stderr}")
        except Exception as e:
            logger.error(f"Failed to run jupytext: {e}")

def start_watching(directories):
    """Start watching the specified directories."""
    observer = Observer()
    handler = JupytextHandler()
    
    for directory in directories:
        abs_path = os.path.abspath(directory)
        if not os.path.isdir(abs_path):
            logger.error(f"Directory not found: {abs_path}")
            continue
            
        logger.info(f"Watching directory: {abs_path}")
        observer.schedule(handler, abs_path, recursive=True)
    
    observer.start()
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        logger.info("Stopping file monitor...")
        observer.stop()
    observer.join()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Monitor directories and run jupytext on changed files.")
    parser.add_argument("directories", nargs="+", help="Directories to monitor")
    parser.add_argument("--log-file", help="Log file path (default: stdout)")
    
    args = parser.parse_args()
    
    if args.log_file:
        file_handler = logging.FileHandler(args.log_file)
        file_handler.setFormatter(logging.Formatter('%(asctime)s - %(message)s'))
        logger.addHandler(file_handler)
        
    logger.info("Starting jupytext file monitor")
    start_watching(args.directories)
