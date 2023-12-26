import spacy
import time
import requests
import re
nlp = spacy.load("en_core_web_sm") # Load the English model for spaCy

def convert_to_meters(value, unit):
    # Basic conversion to meters. Expand or refine as needed.
    unit = unit.lower() # Normalize unit to lowercase for comparison
    if unit in ['meter', 'meters', 'm']:
        return value # Return value as it's already in meters
    elif unit in ['kilometer', 'kilometers', 'km']:
        return value * 1000 # Convert kilometers to meters
    elif unit in ['mile', 'miles']:
        return value * 1609.34 # Convert miles to meters
    return value 

def extract_keywords(text):
    # Extracts keywords from the given text using named entity recognition.
    doc = nlp(text)
    # Initialize a dictionary to store extracted keywords
    keywords = {
        'term': None,
        'location': None,
        'budget': [],
        'reviews': None,
        'open_at': None,
        'radius' : 10000,
        'attributes': []
    }

    for ent in doc.ents:
        # Get the previous token if it exists
        prev_token = doc[ent.start - 1] if ent.start > 0 else None

        # Cuisine 
        if ent.label_ == "NORP":
            keywords['term'] = ent.text
        # Location
        elif ent.label_ == "GPE":
            keywords['location'] = ent.text
        # Budget
        elif ent.label_ == "MONEY":
            budget_value = int(re.findall(r"\d+", ent.text)[0])
            # Map to Yelp's price range
            if budget_value < 30:
                keywords['budget'] = [1]
            elif 30 <= budget_value < 50:
                keywords['budget'] = [1,2]
            elif 50 <= budget_value < 70:
                keywords['budget'] = [1,2,3]
            else:
                keywords['budget'] = [1,2,3,4]
        # Rating
        elif ent.label_ == "CARDINAL" and prev_token and prev_token.text.lower() in ["star", "stars", "review", "reviews"]:
            keywords['reviews'] = ent.text
        # Time - Open
        elif ent.label_ == "TIME":
            keywords['open_at'] = ent.text
        # Radius
        elif ent.label_ == "QUANTITY":
            next_token = doc[ent.start + 1] if ent.start > 0 else None
            if next_token and next_token.text.lower() in ["m", "meter", "meters", "km", "kilometer", "kilometers", "mile", "miles"]:
                try:
                    value = float(ent.text.split()[0])  # Assuming the first part of the entity is the number
                    keywords['radius'] = convert_to_meters(value, next_token.text)
                except ValueError:
                    pass
    return keywords

def get_top_restaurants(latitude, longitude, **keywords):
    # Fetches top restaurants from Yelp API based on specified parameters.
    url = "https://api.yelp.com/v3/businesses/search"
    api_key = "VJ-kW76HYHSjt8Tpr9GV-RsqVIQcEavs3hpVVIyO3tDlewB6FZD2TjxYh7fIRRG9Tb0C0OUPnuzbEojmFHBNjnU6OdLGppiwnc1wU9xWbA2SRwcY0VjZecL17MRiZXYx"
    headers = {"Authorization": f"Bearer {api_key}"}

    # Prepare parameters for the API request
    params = {
        "term": keywords.get('term') + " Restaurants",
        "latitude": float(latitude),
        "longitude" : float(longitude),
        "radius": int(keywords.get('radius', 10000)),
        "limit": keywords.get('limit', 10),
        "sort_by": keywords.get('sort_by', 'rating'),
        "price": keywords.get('budget'),
    }

    # Remove None values from params
    params = {k: v for k, v in params.items() if v is not None}
    response = requests.get(url, headers=headers, params=params)
    # Check if the request was successful
    if response.status_code == 200:
        # Parse the response JSON and return the businesses
        return response.json().get("businesses", [])
    else:
        print(f"Error: {response}")
        return []
    
def get_details(top_restaurants):
    # Extracts and formats details from a list of restaurant data.
    restaurants = []
    for idx, restaurant in enumerate(top_restaurants):
        # Construct a dictionary for each restaurant with relevant details
        new_restaurant = {
            'Name': restaurant['name'],
            'Image': restaurant['image_url'],
            'Rating': restaurant['rating'],
            'Location': restaurant['location']['address1'],
            'City' : restaurant['location']['city'],
            'State' : restaurant['location']['state'],
            'Zip' : restaurant['location']['zip_code'],
            'Phone' : restaurant['display_phone'],
        }
        restaurants.append(new_restaurant)
    return restaurants