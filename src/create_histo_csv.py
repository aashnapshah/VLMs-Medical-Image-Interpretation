import csv
import os
from collections import defaultdict
import os

def append_to_csv(file_path, data):
    fieldnames = list(data.keys())
    file_exists = os.path.isfile(file_path)
    
    with open(file_path, mode='a', newline='') as file:
        writer = csv.DictWriter(file, fieldnames=fieldnames)
        if not file_exists:
            writer.writeheader()
        writer.writerow(data)

def main():
    img_dir = '/home/groups/roxanad/sonnet/ensembleVLMs/Kather_texture_2016_image_tiles_5000'
    output_csv_file_path = '../data/histology/histo_metadata.csv'  
    image_counts = defaultdict(int)
    max_images_per_folder = 625 # this is all the images
    folders = ["STROMA", "TUMOR"] 
    for root, dirs, files in os.walk(img_dir):
        dirs[:] = [d for d in dirs if d[3:] in folders]
        for file in files:
            if file.lower().endswith(('tif')):
                print(file)
                file_path = os.path.join(root, file)
                label = os.path.basename(root)[3:]
                if image_counts[label] >= max_images_per_folder:
                    continue
                image_counts[label] += 1
                row = {"file": file_path}
                row['label'] = label
                append_to_csv(output_csv_file_path, row)


if __name__ == "__main__":
    main()