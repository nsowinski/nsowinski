<?php
    require_once "config.php";
?>

<!DOCTYPE html>
<!--
    Licensed to the Apache Software Foundation (ASF) under one
    or more contributor license agreements.  See the NOTICE file
    distributed with this work for additional information
    regarding copyright ownership.  The ASF licenses this file
    to you under the Apache License, Version 2.0 (the
    "License"); you may not use this file except in compliance
    with the License.  You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing,
    software distributed under the License is distributed on an
    "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
     KIND, either express or implied.  See the License for the
    specific language governing permissions and limitations
    under the License.
-->
<html>
<icon src = "TheTurnDog.png"/>


    <head>
        <meta charset="utf-8">
        <!--
        Customize this policy to fit your own app's needs. For more guidance, see:
            https://github.com/apache/cordova-plugin-whitelist/blob/master/README.md#content-security-policy
        Some notes:
            * gap: is required only on iOS (when using UIWebView) and is needed for JS->native communication
            * https://ssl.gstatic.com is required only on Android and is needed for TalkBack to function properly
            * Disables use of inline scripts in order to mitigate risk of XSS vulnerabilities. To change this:
                * Enable inline JS: add 'unsafe-inline' to default-src
        -->
        <meta http-equiv="Content-Security-Policy" content="default-src 'self' data: gap: https://ssl.gstatic.com 'unsafe-eval'; style-src 'self' 'unsafe-inline'; media-src *; img-src 'self' data: content:;">
        <meta name="format-detection" content="telephone=no">
        <meta name="msapplication-tap-highlight" content="no">
        <meta name="viewport" content="initial-scale=1, width=device-width, viewport-fit=cover">
        <meta name="color-scheme" content="light dark">
        <link rel="stylesheet" href="css/index.css">
        <title>Homepage</title>
    </head>
    <body>
        <div class="app">
            <h1>                
                <embed src = "img/TheTurnDog.PNG" height = "100" width = "100">
            </h1>
            <h2>The Turn Dog</h2>
            <div class = "menu">
                <a href = "post_review.html">New Post</a>
                <a href = "https://twitter.com/TurnDog9">Visit our Twitter Page</a>
                <!--
                <a href="courses_main.php"> Courses </a>
                <a> Friends </a>
                <a href = "login.php">Log In</a>
                <a href = "sign_up.php">Sign Up</a> -->
            </div>
            <div class = "posts">
            <table border = "2">
                <?php
                    $result = $db->query("SELECT * FROM Review");
                    
                    while($row = $result->fetch_assoc()) { ?>
                 <tr>

                        <td> <?php echo $row['name'];?> </td>
                        <td> <?php echo $row['course_name'];?> </td>
                        <td> <?php echo $row['rating'];?> </td>
                        <td> <?php echo $row['review_content'];?> </td>
                        <td> <?php echo $row['date']; ?> </td>
                </tr>
                <tr>
                        <td colspan = "5">
                        <img src="./upload/<?php echo $row['picture_name']; ?>" width="100" height="100">  
                        </td> 
                   <?php
                    }    
                ?>
                 </tr>    
            </table>
            </div>
        <script src="cordova.js"></script>
        <script src="js/index.js"></script>
    </body>
</html>
