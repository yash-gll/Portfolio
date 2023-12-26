import React from "react";
import { useNavigate } from 'react-router-dom';

// Definition of the Options component
const Options = () => {
    // useNavigate hook from react-router-dom for navigation
    const navigate = useNavigate();

    // Function to redirect to a specified path
    const redirectTo = (path) => {
        navigate(path);
    };

    // JSX for rendering the component
    return (
        <div className="options-container">
            <h1>Choose an Option</h1>
            <button onClick={() => redirectTo("/restaurants")}>Restaurants</button>
            <button onClick={() => redirectTo("/outdoor-activities")}>Nearby Outdoor Activities</button>
            <button onClick={() => redirectTo("/events")}>Nearby Events</button>
            <button onClick={() => redirectTo("/finalize")}>Finalize</button>
        </div>
    );
};

// Exporting the Options component for use in other parts of the application
export default Options;
