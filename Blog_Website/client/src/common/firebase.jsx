// Import the functions you need from the SDKs you need
import { initializeApp } from "firebase/app";
import {GoogleAuthProvider, getAuth, signInWithPopup} from 'firebase/auth';

const firebaseConfig = {
  apiKey: "AIzaSyClfv9wP86kuKO5xChF35rwTvoYsCAQCRQ",
  authDomain: "blog-website-835d7.firebaseapp.com",
  projectId: "blog-website-835d7",
  storageBucket: "blog-website-835d7.appspot.com",
  messagingSenderId: "179683799123",
  appId: "1:179683799123:web:e732f8834dd2ac2f3196a6",
  measurementId: "G-H9SDM8S7YK"
};

const app = initializeApp(firebaseConfig);

// Google Authentication
const provider = new GoogleAuthProvider()
const auth = getAuth();

export const authWithGoogle = async () => {
  let user = null;
  await signInWithPopup(auth, provider)
    .then((result) => {
      user = result.user
    })
    .catch((err) => {
      console.log(err);
    })

  return user;
}