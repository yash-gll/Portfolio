import express from 'express';
import mongoose from 'mongoose';
import 'dotenv/config';
import bcrypt from 'bcrypt';
import User from "./Schema/User.js";
import Blog from "./Schema/Blog.js";
import {nanoid} from 'nanoid';
import jwt from 'jsonwebtoken';
import cors from "cors";
import admin from "firebase-admin";
import serviceAccountKey from "./blog-website-835d7-firebase-adminsdk-cylnp-81dfb8d01e.json" assert {type: "json"};
import {getAuth} from "firebase-admin/auth";
import aws from "aws-sdk";

// Node Server
const server = express();

server.use(express.json());
server.use(cors());

// Firebase Server
admin.initializeApp({
    credential: admin.credential.cert(serviceAccountKey)
});

// AWS Server
const s3 = new aws.S3({
    region: 'us-east-2',
    accessKeyId: process.env.AWS_ACCESS_KEY,
    secretAccessKey: process.env.AWS_SECRET_ACCESS_KEY
})

let PORT = 3000;

// Mongo Server
mongoose.connect(process.env.DB_LOCATION, {
    autoIndex: true
})

let emailRegex = /^\w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+$/; // regex for email
let passwordRegex = /^(?=.*\d)(?=.*[a-z])(?=.*[A-Z]).{6,20}$/; // regex for password

const generateUploadURL = async () => {
    const date = new Date();
    const imageName = `${nanoid()}-${date.getTime()}.jpeg`;

    return await s3.getSignedUrlPromise('putObject', {
        Bucket: 'blog-site-yash',
        Key: imageName,
        Expires: 1000,
        ContentType:"image/jpeg" 
    })
}

const formatDatatoSend = (user) => {
    const access_token = jwt.sign({id: user._id}, process.env.SECRET_ACCESS_KEY)
    return{
        access_token: access_token,
        profile_img : user.personal_info.profile_img,
        username : user.personal_info.username,
        fullname : user.personal_info.fullname
    }
}

const generateUsername = async(email) => {
    let username = email.split("@")[0];
    let isUsernameUnique = await User.exists({"personal_info.username":username}).then((result) => result)
    console.log(isUsernameUnique);
    if(isUsernameUnique !== null){
        username += nanoid().substring(0, 5)
    }
    return username
}

const verifyJWT = (req, res, next) => {
    const authHeader = req.headers['authorization'];
    const token = authHeader && authHeader.split(" ")[1];
    if (token== null){
        return res.status(401).json({error: "No access token"})
    }

    jwt.verify(token, process.env.SECRET_ACCESS_KEY, (err, user) => {
        if(err){
            return res.status(403).json({error: "Access token is invalid"})
        }

        req.user = user.id;
        next()
    })
}

server.get("/get-upload-url", (req, res) => {
    generateUploadURL().then(url => res.status(200).json({uploadURL: url}))
    .catch(err => {
        console.log(err.message);
        return res.status(500).json({error: err.message})
    })
})

server.post("/sign-up", (req, res) => {
    let {fullname, email, password} = req.body
    // Validating the data-form
    if(fullname.length < 3){
        return res.status(403).json({"error": "Fullname must be at least 3 characters long"})
    }
    if(!email.length){
        return res.status(403).json({"error": "Enter Email"})
    }
    if(!emailRegex.test(email)){
        return res.status(403).json({"error": "Please enter a valid email"})
    }
    if(!passwordRegex.test(password)){
        return res.status(403).json({"error": "Password should be 6 to 20 characters long with a numeric, 1 lowercase and 1 uppercase letters"})
    }

    bcrypt.hash(password, 10, async(err, hashed_password) => {
        let username = await generateUsername(email);
        console.log(username);
        let user = new User({
            personal_info: {fullname, email, password: hashed_password, username}
        })
        user.save().then((u) => {
            return res.status(200).json(formatDatatoSend(user))
        })
        .catch(err => {
            if(err.code == 11000){
                return res.status(500).json({"error": "Email already exists"})
            }
            return res.status(500).json({"error": err.message})
        })
    })
})

server.post("/sign-in", (req, res) => {
    let {email, password} = req.body;
    User.findOne({"personal_info.email": email})
        .then((user) => {
            if(!user){
                return res.status(403).json({"error": "Email not found"});
            }
            if(user.google_auth){
                bcrypt.compare(password, user.personal_info.password, (err, result) => {
                    if(err){
                        return res.status(403).json({"error": "Error occured while login please try again"});
                    }
                    if(!result){
                        return res.status(403).json({"error": "Incorrect Password"});
                    } else{
                        return res.status(200).json(formatDatatoSend(user));
                    }
                })
            } else {
                return res.status(403).json({"error": "Account was created using Google. Try logging in with google."})
            }
        })
        .catch(err => {
            console.log(err);
            return res.status(500).json({"error": err.message})
        })
})

server.post("/google-auth", async(req, res) => {
    let {access_token} = req.body;
    getAuth()
        .verifyIdToken(access_token)
        .then(async (decodecUser) => {
            let {email, name, picture} = decodecUser;
            picture = picture.replace("s96-c", "s384-c");
            let user = await User.findOne({"personal_info.email": email}).select("personal_info.fullname personal_info.username personal_info.profile_img google_auth").then((u) => {
                return u || null
            })
            .catch(err => {
                return res.status(500).json({"error": err.message});
            })

            if(user){
                if(!user.google_auth){
                    return res.status(403).json({"error": "This email was signed up without google. Please log in with password to access the account"});
                }
            } else{
                let username = await generateUsername(email);
                user = new User({
                    personal_info : {fullname: name, email, profile_img: picture, username},
                    google_auth: true
                })
                await user.save().then((u) => {
                    user = u;
                })
                .catch(err => {
                    return res.status(500).json({"error": err.message});
                })
            }
            return res.status(200).json(formatDatatoSend(user));
        })
        .catch(err => {
            return res.status(500).json({"error": "Failed to authenticate you with google. Try with some other google account"})
        })
})

server.post('/create-blog', verifyJWT, (req, res) => {
    
    let authorId = req.user;
    let {title, des, banner, tags, content, draft} = req.body;

    if(!title.length){
        return res.status(403).json({error: "You must provide a title"})
    }

    if(!draft){
        if(!title.length){
            return res.status(403).json({error: "You must provide a title to publish the blog"})
        }
        if(!des.length || des.length > 200){
            return res.status(403).json({error: "You must provide a blog description under 200 characters"})
        }
        if(!banner.length){
            return res.status(403).json({error: "You must provide a blog banner to publish the blog"})
        }

        if(!content.blocks.length){
            return res.status(403).json({error: "You must provide blog content to publish the blog"})
        }
        if(!tags.length || tags.length > 10){
            return res.status(403).json({error: "You must provide tags to publish the blog (Maximum 10)"})
        }
    }

    tags = tags.map(tag => tag.toLowerCase());
    let blog_id = title.replace(/[^a-zA-Z0-9]/g, ' ').replace(/\s+/g,"-").trim() + nanoid();
    let blog = new Blog({
        title, des, banner, content, tags, author: authorId, blog_id, draft: Boolean(draft)
    })

    blog.save().then(blog => {
        let incrementVal = draft ? 0: 1;
        User.findOneAndUpdate({_id: authorId}, {$inc: {"account_info.total_posts": incrementVal}, $push: {"blogs": blog_id}}).then(user => {
            return res.status(200).json({id: blog.blog_id})
        })
        .catch(err => {
            return res.status(500).json({error: "Failed to update total posts number"})
        })
    })
    .catch(err => {
        return res.status(500).json({error: err.message})
    })
    return res.json({status: 'done'});
})

server.listen(PORT, () => {
    console.log('listening on port ->' + PORT)
})