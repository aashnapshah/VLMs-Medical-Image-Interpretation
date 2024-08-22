import csv
import os
from collections import defaultdict
from matplotlib import pyplot as plt
import numpy as np

def calculate_average_intensity(file_path):
    img = plt.imread(file_path)
    gray = img.mean(axis=2)
    return np.mean(gray)

def update_csv_with_intensity(input_csv_path, output_csv_path):
    with open(input_csv_path, 'r') as input_file, open(output_csv_path, 'w', newline='') as output_file:
        reader = csv.DictReader(input_file)
        fieldnames = reader.fieldnames + ['average_intensity']
        
        writer = csv.DictWriter(output_file, fieldnames=fieldnames)
        writer.writeheader()
        
        for row in reader:
            if 'average_intensity' not in row:
                file_path = row['file']
                if os.path.exists(file_path) and file_path.lower().endswith(('tif')):
                    average_intensity = calculate_average_intensity(file_path)
                    row['average_intensity'] = average_intensity
                else:
                    row['average_intensity'] = -1
            writer.writerow(row)

def main():
    img_dir = '/home/groups/roxanad/sonnet/ensembleVLMs/Kather_texture_2016_image_tiles_5000'
    input_csv_file_path = '../data/histology/histo_metadata.csv'
    output_csv_file_path = '../data/histology/histo_metadata_new.csv'
    
    update_csv_with_intensity(input_csv_file_path, output_csv_file_path)
    
    print(f"Updated CSV saved to {output_csv_file_path}")

if __name__ == "__main__":
    main()