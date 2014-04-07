import mock
import os
import shutil
import sys
import unittest

HOMEDIR = os.path.expanduser("~")

categories = {
    '.gif': 'gifs',
    '.jpg': 'gifs',
    '.png': 'screenshots',
    '.pdf': 'pdfs',
}


class CategorizeStuffTests(unittest.TestCase):

    def test_get_category(self):
        import tempfile
        file = tempfile.mktemp() + '.png'
        self.assertEqual(get_category(file), 'screenshots')


    def test_get_category_no_category(self):
        import tempfile
        file =tempfile.mktemp()
        self.assertIsNone(get_category(file))


    @mock.patch.object(shutil, 'move')
    def test_move_file_to_its_category(self, move_method):
        cat = 'rndm'
        file = 'randomfile'

        move_file_to_its_category(file, cat)

        move_method.assert_called_once_with(os.path.join(HOMEDIR, file), os.path.join(HOMEDIR, cat))


def get_category(name):
    _name, ext = os.path.splitext(name)
    return categories.get(ext)


def move_file_to_its_category(name, category):
    fullname = os.path.join(HOMEDIR, name)
    fullcategory = os.path.join(HOMEDIR, category)
    shutil.move(fullname, fullcategory)


if __name__ == '__main__':
    if "test" in sys.argv:
        sys.argv.pop(sys.argv.index('test'))
        unittest.main()         # run tests
    else:
        names = os.listdir(HOMEDIR)
        for name in names:
            cat = get_category(name)
            if cat:
                print 'Moving file %s to %s' % (name, cat)
                move_file_to_its_category(name, cat)
        print 'Done.'
