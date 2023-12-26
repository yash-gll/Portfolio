import pandas as pd
import requests
import numpy as np
import re
import os
import google.generativeai as palm
import openai
from csv import field_size_limit
from langchain.document_loaders.csv_loader import CSVLoader
from langchain.embeddings.openai import OpenAIEmbeddings
from langchain.prompts import PromptTemplate
from langchain.chat_models import ChatOpenAI
from langchain.chains import LLMChain

openai.api_key = "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"

def geocode_location(location):
    # Function to convert a location name to latitude and longitude using Google Geocoding API
    api_key = "AIzaSyBzy3SjM5_icsmz9xhBpLGO19ShKgZv2l8"  # Make sure to set your API key in your environment variables
    url = "https://maps.googleapis.com/maps/api/geocode/json"

    params = {
        "address": location,
        "key": api_key
    }

    response = requests.get(url, params=params)
    if response.status_code == 200:
        results = response.json()["results"]
        if results:
            geometry = results[0]["geometry"]["location"]
            return geometry["lat"], geometry["lng"]  # returns a dict with 'lat' and 'lng' keys
        else:
            raise ValueError("No results found for the specified location.")
    else:
        raise ConnectionError(f"Failed to fetch data: {response.status_code}, {response.reason}")
    
def create_locations(places):
    # Creates a dataframe of selected locations with types, addresses, names, and ratings
    selected = []
    for p in places:
        s = []
        if(p['type'] == 'restaurant'):
            s.append("Restaurant")
            s.append(p['Location'] + ", " + p['City'] + ", " + p['State'] + ", " + p['Zip'])
        else:
            s.append("Attraction")
            s.append(p['Location'])
        s.append(p['Name'])
        s.append(p['Rating'])
        selected.append(s)
    
    activities = pd.DataFrame(selected, columns=['Type', 'Address', 'Name', 'Rating'])
    activities['Latitude'], activities['Longitude'] = zip(*activities['Address'].apply(geocode_location))
    file_path = '/Users/yashgoel/Desktop/ATDS/Portfolio/Itenary_Planer_LLM/server/activities.csv'
    print("Activities CSV Saved")
    activities.to_csv(file_path, header=True, index=False)

def create_matrix(locations):
    # Generates a distance and duration matrix between locations using Google Distance Matrix API
    activities = pd.read_csv('/Users/yashgoel/Desktop/ATDS/Portfolio/Itenary_Planer_LLM/server/activities.csv')
    distance_matrix = np.zeros((len(locations), len(locations)))
    duration_matrix = np.zeros((len(locations), len(locations)))
    api_key = "AIzaSyBzy3SjM5_icsmz9xhBpLGO19ShKgZv2l8"
    # Calculate distances for each pair of locations
    for i, loc1 in enumerate(locations):
        for j, loc2 in enumerate(locations):
            if i != j:
                url = f"https://maps.googleapis.com/maps/api/distancematrix/json?origins={loc1[0]},{loc1[1]}&destinations={loc2[0]},{loc2[1]}&key={api_key}"
                response = requests.get(url)
                if response.status_code == 200:
                    result = response.json()
                    # Extract distance from the response
                    distance = result['rows'][0]['elements'][0]['distance']['value']
                    duration = result['rows'][0]['elements'][0]['duration']['value']
                    distance_matrix[i][j] = distance
                    duration_matrix[i][j] = duration
                else:
                    distance_matrix[i][j] = np.nan
    # Creating and saving the distance and duration dataframes
    distance_matrix_df = pd.DataFrame(distance_matrix, index=activities['Name'], columns=activities['Name'])
    duration_matrix_df = pd.DataFrame(duration_matrix, index=activities['Name'], columns=activities['Name'])
    # Convert matrix data to a list of dictionaries for easy CSV conversion
    distance_list = []
    for i, origin in enumerate(activities['Name']):
        for j, destination in enumerate(activities['Name']):
            if i != j:
                distance_list.append({
                    'origin': origin,
                    'destination': destination,
                    'distance': distance_matrix_df.iloc[i, j]
                })
    
    duration_list = []

    for i, origin in enumerate(activities['Name']):
        for j, destination in enumerate(activities['Name']):
            if i != j:
                duration_list.append({
                    'origin': origin,
                    'destination': destination,
                    'duration': duration_matrix_df.iloc[i, j]
                })
    
    distance = pd.DataFrame(distance_list)
    duration = pd.DataFrame(duration_list)

    distance.to_csv('/Users/yashgoel/Desktop/ATDS/Portfolio/Itenary_Planer_LLM/server/distance.csv', header=True, index=False)
    duration.to_csv('/Users/yashgoel/Desktop/ATDS/Portfolio/Itenary_Planer_LLM/server/duration.csv', header=True, index=False)
    print("Matrix CSV Saved")

def gpt_generate(start, end, duration):
    # Vectorize
    loader = CSVLoader("/Users/yashgoel/Desktop/ATDS/Portfolio/Itenary_Planer_LLM/server/activities.csv")
    documents = loader.load()
    embeddings = OpenAIEmbeddings(openai_api_key = "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")

    # LLMChain
    llm = ChatOpenAI(temperature=0, model="gpt-4-1106-preview", openai_api_key = "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" )

    template = """
    You are a personalized itinerary planner for a trip. Use the {activities} for location data.

    The rules for generating the itinerary are as follows:
    1/ You must cover at least 1 activity between each meal with enough time to travel for one place to another. 
    2/ The locations for the activities must be chosen from given locations. 
    3/ Select at least three locations of type "restaurant" for breakfast, lunch, and dinner. 
    4/ Minimize travel time between activities. 5/ Ensure that the selected activities fit within the specified trip duration.

    Here is the prompt I received from the user:

    Start Date: {start_date}
    End Date: {end_date}
    Duration of Stay: {duration}
    Prompt: {prompt}

    Give output in this format: For each day from start date to end date, give a list of activities, followed by the start and end time for the activity."""

    prompt = PromptTemplate(
        input_variables = ["activities", "start_date", "end_date", "duration", "prompt"],
        template=template
    )

    chain = LLMChain(llm=llm, prompt=prompt)
    response = chain.run(activities=documents, start_date=start, end_date=end, duration=duration, prompt="Generate itinerary for the given duration")
    print(response)
    final_response = process(response)
    return final_response

def bard_generate(start, end, duration):
    # Set Google API key
    os.environ['GOOGLE_API_KEY'] = 'AIzaSyBRYFVYLYYcfjnDYKVs6tqyUVCN_4tmf84'
    google_api_key = os.getenv('GOOGLE_API_KEY')
    palm.configure(api_key=google_api_key)

    # Load documents
    loader = CSVLoader("/Users/yashgoel/Desktop/ATDS/Portfolio/Itenary_Planer_LLM/server/activities.csv")
    documents = loader.load()

    template = """
    You are a personalized itinerary planner for a trip. Use the following activities for location data: {activities}.

    The rules for generating the itinerary are as follows:
    1. You must cover at least 1 activity between each meal with enough time to travel from one place to another.
    2. The locations for the activities must be chosen from given locations.
    3. Select at least three locations of type "restaurant" for breakfast, lunch, and dinner.
    4. Minimize travel time between activities.
    5. Ensure that the selected activities fit within the specified trip duration.

    Here is the prompt I received from the user:

    Start Date: {start_date}
    End Date: {end_date}
    Duration of Stay: {duration}
    Prompt: {user_prompt}
    """

    # Fill the template with actual data
    filled_prompt = template.format(
        activities=str(documents),
        start_date=start,
        end_date=end,
        duration=duration,
        user_prompt="Generate itinerary for the given duration"
    )

    # Generate text using PALM
    completion = palm.generate_text(
        model='models/text-bison-001',
        prompt=filled_prompt,
        temperature=0.1
    )

    print(completion.result)
    final_response = process(completion.result)
    return final_response


def process(generated_text):
    day_texts = generated_text.split("Day ")[1:]
    itinerary_data = []
    for day_text in day_texts:
        day_lines = day_text.strip().split("\n")
        item_list = []
        for dl in day_lines[1:]:
            if dl == "" or dl =="**":
                continue
            else:
                item_list.append(dl)
        itinerary_data.append({"day": f"Day {day_lines[0]}", "items": item_list})

    # Convert the itinerary data to JSON
    import json

    json_data = json.dumps(itinerary_data, indent=2)
    return json_data