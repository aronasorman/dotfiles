import datetime
import os.path
import subprocess
import sys

def is_weekly_backup(backup_weekday=5):
    return datetime.date.today().weekday() == backup_weekday

def archive_name(date=None):
    if not date:
        date = datetime.date.today()

    if is_weekly_backup():
        return date.strftime("Weekly %W/%m/%Y")
    else:
        return date.strftime("Daily %d/%m/%Y")

def tarsnap(mode, archive=None, keyfile="~/crypt/write-only.key", files=None):
    keyfile = os.path.expanduser(keyfile)
    arglist = ["tarsnap", mode,
               "--keyfile", keyfile]
    if archive and mode == '-c':
        arglist += ["-f", archive]
        arglist += files
    return subprocess.check_output(arglist)

def list_archives():
    return tarsnap("--list-archives", keyfile="~/all.key").rstrip().split('\n')

def archive_to_replace(archive_date=None):
    if archive_date == None:
        archive_date = datetime.date.today()

    # explanation: we do every day of the week backups
    # as well as weekly backups which span the whole year.
    days_from_last_backup = 365 if is_weekly_backup else 7
    archive_date_to_delete = archive_date - datetime.timedelta(days=days_from_last_backup)
    archive_to_delete = archive_name(date=archive_date_to_delete)

    if archive_to_delete in list_archives():
        return archive_to_delete
    else:
        return None

def delete_archive(archive):
    tarsnap("-d", keyfile="~/all.key", archive=archive)

def create_archive(arch, files):
    tarsnap("-c", archive=arch, files=files)

def backup(files):
    name = archive_name()
    to_replace = archive_to_replace()
    if to_replace:
        print "Deleting archive %s" % to_replace
        delete_archive(to_replace)

    print "Creating archive %s" % name
    create_archive(name, files)
    print "Archive created."

if __name__ == '__main__':
    print "Initiating backup."
    backup(sys.argv[1:])
