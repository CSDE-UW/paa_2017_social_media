import os
import re
import csv
import glob
import pandas as pd
from datetime import datetime


def merge_subdirectories(input_folder_path, output_filename):
    """
    Cleans and processes multiple .csvs
    Creates new merged .csv file
    folder_path: string of top-level file folder with subfolders
    output_filename: string name for merged .csv file
    output_filename is placed one directory above the input_folder_path
    """
    current_dir = os.getcwd()
    os.chdir(current_dir+"/"+input_folder_path)
    folder_path = os.getcwd()
    os.chdir('../')
    output_folder_path = os.getcwd()
    try:
        os.remove(output_filename)
    except OSError:
        pass

    filewriter = csv.writer(open(output_filename, 'wb'))
    file_counter = 0

    for folder in os.listdir(folder_path):
        data_path = (folder_path+"/"+folder+"/"+"api_requests_csv")
        os.chdir(data_path)
        for input_file in glob.glob(os.path.join(data_path, '*.csv')):
                with open(input_file, 'rU') as csv_file:
                        filereader = csv.reader(csv_file)
                        if file_counter < 1:
                                for row in filereader:
                                        filewriter.writerow(row)
                        else:
                                header = next(filereader, None)
                                for row in filereader:
                                        filewriter.writerow(row)
                file_counter += 1
        print("finished %s folder at %s" %
              (folder, datetime.now().strftime('%Y-%m-%d %H:%M:%S')))
    print("saved %s in %s" % (output_filename, output_folder_path))
    os.chdir(current_dir)


def process_facebook_data(df):
    df = df[["state", "name", "ages_ranges", "genders", "behavior",
             "audience"]]
    gender_map = {'1.0': 'female', '2.0': 'male', '0.0': 'total population'}
    age_map = {'13': 'ages_13_65', '15': 'ages_15_19', '20': 'ages_20_24',
               '25': 'ages_25_29', '30': 'ages_30_34', '35': 'ages_35_39',
               '40': 'ages_40_44', '45': 'ages_45_49', '50': 'ages_50_54',
               '55': 'ages_55_59', '60': 'ages_60_65'}
    df["behavior"] = df.loc[:, ("behavior")].apply(lambda x: re.findall(r'\d+',x))
    df["behavior"] = df.loc[:, ("behavior")].apply(lambda x: ''.join(x))
    df["genders"] = df.loc[:, ("genders")].astype('string')
    df["genders"] = df.loc[:, ("genders")].replace(gender_map)
    df["ages_ranges"] = df["ages_ranges"].apply(lambda x: age_map[x[-3:-1]])
    df["genders"] = df.loc[:, ("genders")].astype('string')
    df["genders"] = df.loc[:, ("genders")].replace(gender_map)
    df = df[["state", "name", "ages_ranges", "genders",
             "behavior", "audience"]]
    return(df)
