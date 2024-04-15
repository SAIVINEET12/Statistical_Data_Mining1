import csv
import time

import requests
from pyquery import PyQuery


def populate_phone_detail(phone_link, phones_data):
    details = requests.get("https://www.gsmarena.com/" + phone_link)
    phone_details = PyQuery(details.text)
    phone_name = phone_details.find(".specs-phone-name-title").text()
    specs = phone_details.find("#specs-list")
    feature_categories = specs.find("table")

    for feature_category in feature_categories.items():
        features = feature_category.find("tr")
        for feature in features.items():
            feature_name = feature.find("td").eq(0).find("a").text()
            feature_value = feature.find("td").eq(1).text()
            if feature_name in phones_data:
                phones_data[feature_name].append(feature_value)
            else:
                phones_data[feature_name] = [feature_value]

    phones_data["name"] = [phone_name]


def get_phone_details(link):
    phone_details = {}
    phones_response = requests.get(link)
    root = PyQuery(phones_response.text)
    phones = root("#review-body").find("ul").find("li")

    for phone in phones.items():
        time.sleep(20)
        phone_link = phone.find("a").attr("href")
        populate_phone_detail(phone_link, phone_details)

    return phone_details


def create_csv(file_name, data):
    with open(file_name, "wb") as output:
        writer = csv.writer(output)
        writer.writerow(data.keys())
        writer.writerows(zip(*data.values()))


if __name__ == '__main__':
    phone_data = get_phone_details("https://www.gsmarena.com/samsung-phones-9.php")
    create_csv("data.csv", phone_data)
