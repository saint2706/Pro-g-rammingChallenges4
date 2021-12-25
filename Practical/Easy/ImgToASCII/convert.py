import pywhatkit as pwk
import easygui as eg


source_path = eg.fileopenbox()
destination_path = source_path.split(".")[0]

pwk.image_to_ascii_art(source_path, destination_path)
