import './App.css';
import React from "react";
import { BrowserRouter as Router, Route, Routes } from "react-router-dom";
import LandingPage from "./Pages/Landing";
import Options from "./Pages/Options";
import Restaurants from './Pages/Restaurants';
import Events from './Pages/Events';
import OutdoorActivities from './Pages/OutdoorActivities';
import Finalize from './Pages/Finalize';
import GPT from './Pages/GPT';
import BARD from './Pages/Bard';

const App = () => {
  return (
      <Router>
          <Routes>
            <Route path="/" element={<LandingPage />} />
            <Route path="/Options" element={<Options />} />
            <Route path="/restaurants" element={<Restaurants />} />
            <Route path="/events" element={<Events />} />
            <Route path="/outdoor-activities" element={<OutdoorActivities />} />
            <Route path="/finalize" element={<Finalize />} />
            <Route path="/gpt" element={<GPT />} />
            <Route path="/Bard" element={<BARD />} />
          </Routes>
      </Router>
  );
};

export default App;
