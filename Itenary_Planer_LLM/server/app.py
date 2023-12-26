from flask import Flask, request, jsonify
from flask_cors import CORS
from pymongo.mongo_client import MongoClient
from pymongo.server_api import ServerApi
import yelp as yelp
import pandas as pd
import activities as activities
import LLM as LLM

app = Flask(__name__)
CORS(app) # Enable CORS (Cross-Origin Resource Sharing)

# MongoDB setup
mongo_uri = "mongodb+srv://ygoel2609:Promax1239@atds.6ldydbq.mongodb.net/?retryWrites=true&w=majority"
mongo_client = MongoClient(mongo_uri, server_api=ServerApi('1'))
db = mongo_client["ATDS"]  # Replace with your database name
chat_collection = db["chats"]
selected_collection = db["selected"]

# Check MongoDB connection
try:
    mongo_client.admin.command('ping')
    print("Pinged your deployment. You successfully connected to MongoDB!")
except Exception as e:
    print(e)

# Functions to interact with the MongoDB collections
def save_chat_message(message, origin, type):
    # Save a chat message to the 'chats' collection
    chat_collection.insert_one({"type": type, "text": message, "sender": 'user', "origin": origin})

def save_chat_messages(type, data, origin):
    # Save a response from the server to the 'chats' collection
    chat_collection.insert_one({"type": type, "data": data, "sender": 'server', "origin": origin})

def get_chat_history(origin):
    # Retrieve chat history based on origin
    return list(chat_collection.find({"origin": origin}, {"_id": 0, "type": 1, "text": 1, "data": 1, "sender": 1}))

# Route to process messages
@app.route('/process-message', methods=['POST',  'OPTIONS'])

def process_message():
    if request.method == 'OPTIONS':
        headers = {
            'Access-Control-Allow-Origin': '*',
            'Access-Control-Allow-Methods': 'POST',
            'Access-Control-Allow-Headers': '*'
        }
        return '', 204, headers
    
    if request.method == 'POST':
        print("Received message:", request.json)
        msg = request.json['message']
        origin = request.json['origin']
        latitude = request.json['latitude']
        longitude = request.json['longitude']

        if origin == "Restaurant":
            save_chat_message(msg, origin, type="type1")
            params = yelp.extract_keywords(msg)
            top_resturants = yelp.get_top_restaurants(latitude, longitude, **params)
            resturants = yelp.get_details(top_resturants)

            # Save each restaurant data to MongoDB
            for restaurant in resturants:
                save_chat_messages('restaurant', restaurant, origin)
            return jsonify({'restaurants': resturants})
        
        if origin == "Outdoor":
            save_chat_message(msg, origin, type="type2")
            places = activities.classify_prompt(msg)
            top_places = activities.find_nearby_attractions(places, latitude, longitude)
            outdoor_places = activities.get_details(top_places)
            
            # Save each restaurant data to MongoDB
            for place in outdoor_places:
                save_chat_messages('outdoor', place, origin)
                print(place)
            return jsonify({'outdoor': outdoor_places})

# Route to get chat history
@app.route('/get-chat-history', methods=['GET'])
def get_chat_history_route():
    origin = request.args.get('origin')
    history = get_chat_history(origin)
    return jsonify(history)

# Route to process location
@app.route('/process-location', methods=['POST'])
def process_location():
    data = request.json
    location = data.get('location')
    print(location)
    lat, lng = activities.geocode_location(location)
    return jsonify({'latitude': lat, 'longitude': lng})

# Routes for selecting and deselecting activities
@app.route('/select-activity', methods=['POST'])
def select_activity():
    data = request.json
    activity = data['activity']
    is_selected = data['isSelected']

    if is_selected:
        # Add to the 'selected' collection
        activity['type'] = "activity"
        selected_collection.insert_one(activity)
        return jsonify({"status": "success", "message": "Activity added to selected"})
    else:
        # Remove from the 'selected' collection
        # Assuming 'activity' contains a unique identifier, like 'Name'
        result = selected_collection.delete_one({"Name": activity['Name']})
        if result.deleted_count > 0:
            return jsonify({"status": "success", "message": "Activity removed from selected"})
        else:
            return jsonify({"status": "failure", "message": "Activity not found in selected"})
        
    return jsonify({"status": "failure", "message": "Invalid request"})

# Routes for selecting and deselecting restaurants
@app.route('/select-restaurant', methods=['POST'])
def select_restaurant():
    data = request.json
    restaurant = data['restaurant']
    is_selected = data['isSelected']

    if is_selected:
        # Add to the 'selected' collection
        restaurant['type'] = "restaurant"
        selected_collection.insert_one(restaurant)
        return jsonify({"status": "success", "message": "Restaurant added to selected"})
    else:
        # Remove from the 'selected' collection
        # Assuming 'activity' contains a unique identifier, like 'Name'
        result = selected_collection.delete_one({"Name": restaurant['Name']})
        if result.deleted_count > 0:
            return jsonify({"status": "success", "message": "Restaurant removed from selected"})
        else:
            return jsonify({"status": "failure", "message": "Restaurant not found in selected"})
        
    return jsonify({"status": "failure", "message": "Invalid request"})

# Routes for selecting items
@app.route('/get-selected', methods=['GET'])
def get_selected():
    selected_items = list(selected_collection.find({}))
    
    # Manually convert ObjectId to string
    for item in selected_items:
        item['_id'] = str(item['_id'])
    x = jsonify(selected_items)
    return jsonify(selected_items)

# Routes for generating itinerary through GPT
@app.route('/generate_itinerary_1', methods=['GET', 'POST'])
def generate_itinerary_1():
    selected_items = list(selected_collection.find({}))
    for item in selected_items:
        item['_id'] = str(item['_id'])

    start = request.json['startDate']
    end = request.json['endDate']
    duration = request.json['duration']
    print(start, end, duration)
    # print("Converting to CSV's")
    # LLM.create_locations(selected_items)

    # activities = pd.read_csv("/Users/yashgoel/Desktop/ATDS/Portfolio/Itenary_Planer_LLM/server/activities.csv")
    # locations = activities[['Latitude', 'Longitude']].values.tolist()
    # LLM.create_matrix(locations)
    response = LLM.gpt_generate(start, end, duration)
    print(response)
    return jsonify({"message": response})

# Routes for generating itinerary through BARD
@app.route('/generate_itinerary_2', methods=['GET', 'POST'])
def generate_itinerary_2():
    selected_items = list(selected_collection.find({}))
    for item in selected_items:
        item['_id'] = str(item['_id'])

    start = request.json['startDate']
    end = request.json['endDate']
    duration = request.json['duration']
    print(start, end, duration)
    # print("Converting to CSV's")
    # LLM.create_locations(selected_items)

    # activities = pd.read_csv("/Users/yashgoel/Desktop/ATDS/Portfolio/Itenary_Planer_LLM/server/activities.csv")
    # locations = activities[['Latitude', 'Longitude']].values.tolist()
    # LLM.create_matrix(locations)
    response = LLM.bard_generate(start, end, duration)
    print(response)
    return jsonify({"message": response})

if __name__ == '__main__':
    app.run(debug=True, port=5000)
