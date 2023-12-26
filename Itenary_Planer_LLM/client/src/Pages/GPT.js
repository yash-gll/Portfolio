import React, { useState, useEffect } from 'react';

const GPT = () => {
    // State to store the generated text
    const [generatedText, setGeneratedText] = useState('');

    useEffect(() => {
        // Retrieve start and end dates from session storage
        const startDate = sessionStorage.getItem('startDate');
        const endDate = sessionStorage.getItem('endDate');

        // Calculate duration of stay
        const diffInMs = new Date(endDate).getTime() - new Date(startDate).getTime();
        const diffInDays = Math.round(diffInMs / (1000 * 60 * 60 * 24));
        const duration = diffInDays >= 0 ? `${diffInDays} days` : 'Invalid date range';
        
        // Prepare data to send in the POST request
        const data = {
            startDate,
            endDate,
            duration
        };

        // Function to fetch generated text from a server
        const fetchGeneratedText = async () => {
            try{
                const response = await fetch('http://127.0.0.1:5000/generate_itinerary_1', {
                    method: 'POST', 
                    headers: {
                        'Content-Type': 'application/json',
                    },
                    body: JSON.stringify(data),
                })
                if (response.ok) {
                    console.log("Response Recieved");
                    const data = await response.json();
                    console.log(data.message);
                    setGeneratedText(data.message);
                } else {
                    console.error('Request failed');
                }
            } catch (error) {
                console.error('Error fetching generated text:', error);
            }
        };
        fetchGeneratedText();
    }, []);

    // Function to render the itinerary
    const renderItinerary = (itinerary) => {
        return (
            <div>
                <h1>Generated Itinerary</h1>
                {itinerary.map((dayData, index) => (
                    <div key={index}>
                        <h2>{dayData.day}</h2>
                        <ul>
                            {dayData.items.map((item, itemIndex) => (
                                <li key={itemIndex}>
                                    <p>{item}</p>
                                </li>
                            ))}
                        </ul>
                    </div>
                ))}
            </div>
        );
    };

    return (
        // Render the component
        <div>
            {generatedText ? renderItinerary(JSON.parse(generatedText)) : <p>Loading...</p>}
        </div>
    );
};

export default GPT;
