import importlib.util
import sys
import unittest
from pathlib import Path

BASE_DIR = Path(__file__).resolve().parents[1]
MODULE_PATH = BASE_DIR / "converter.py"
TEMPLATE_DIR = BASE_DIR / "templates"

spec = importlib.util.spec_from_file_location("markdown_editor.converter", MODULE_PATH)
module = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = module
assert spec.loader is not None
spec.loader.exec_module(module)

TemplateManager = module.TemplateManager
convert_markdown = module.convert_markdown
convert_to_xml = module.convert_to_xml
parse_front_matter = module.parse_front_matter


class ConverterTests(unittest.TestCase):
    def setUp(self) -> None:
        self.manager = TemplateManager(TEMPLATE_DIR)

    def test_front_matter_parsing(self) -> None:
        metadata, body = parse_front_matter("""---\ntitle: Sample\nauthor: Alice\n---\n\n# Heading\n""")
        self.assertEqual(metadata["title"], "Sample")
        self.assertEqual(metadata["author"], "Alice")
        self.assertTrue(body.startswith("# Heading"))

    def test_conversion_applies_template(self) -> None:
        text = """---\ntitle: Doc\ntemplate: dark\n---\n\n# Hello\n\nSome **bold** text."""
        result = convert_markdown(text, template_manager=self.manager)
        self.assertIn("<h1>Doc</h1>", result.html)
        self.assertIn("<strong>bold</strong>", result.html)
        self.assertEqual(result.template_used, "dark")

    def test_xml_conversion_contains_metadata(self) -> None:
        text = """---\ntitle: XML Demo\nauthor: Bob\n---\n\nContent line."""
        result = convert_markdown(text, template_manager=self.manager, preferred_template="default")
        xml_payload = convert_to_xml(result)
        self.assertIn("<title>XML Demo</title>", xml_payload)
        self.assertIn("<content_html>", xml_payload)


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
