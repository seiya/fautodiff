import json
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import fadmod


class TestFadmod(unittest.TestCase):
    def test_roundtrip(self):
        data = {
            "routines": {"foo": {"module": "m", "name_fwd_ad": "foo_fwd_ad"}},
            "variables": {"x": {"typename": "real", "constant": True}},
        }
        fm = fadmod.FadmodV1.from_dict(data)
        with TemporaryDirectory() as tmp:
            path = Path(tmp) / "test.fadmod"
            fm.write(path)
            loaded = fadmod.FadmodBase.load(path)
        self.assertIsInstance(loaded, fadmod.FadmodV1)
        self.assertEqual(loaded.dump(), fm.dump())

    def test_unknown_version(self):
        with TemporaryDirectory() as tmp:
            path = Path(tmp) / "bad.fadmod"
            path.write_text(json.dumps({"version": 99}))
            with self.assertRaisesRegex(RuntimeError, "unsupported fadmod version 99"):
                fadmod.FadmodBase.load(path)

    def test_invalid_variable_entry(self):
        data = {"routines": {}, "variables": {"x": {"constant": True}}}
        with self.assertRaisesRegex(RuntimeError, "missing 'typename'"):
            fadmod.FadmodV1.from_dict(data)


if __name__ == "__main__":
    unittest.main()
