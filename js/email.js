function reverse(str) {
    var rev = "";
    for (var i = 0; i < str.length; i++) {
        rev += str.charAt(str.length-1 - i);
    }
    return rev;
}

window.onload = function() {
    var emailStr = "ude.nnepu.saes@egnad";
    
    // #contact-list > ul
    var contact_list = document.getElementById("contact-list").firstElementChild;

    emailStr = reverse(emailStr);
    var emailElem = document.createElement("li");
    var emailHtml = "<a href=\"mailto:" + emailStr + "\">Email</a>";
    emailElem.innerHTML = emailHtml;

    contact_list.insertBefore(emailElem, null);
};
