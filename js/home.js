function reverse(str) {
    var rev = "";
    for (var i = 0; i < str.length; i++) {
        rev += str.charAt(str.length-1 - i);
    }
    return rev;
}

window.onload = function() {
    var emailStr = "ude.nnepu.saes@egnad";
    var emailElem = document.getElementById("email");
    emailStr = reverse(emailStr);
    emailElem.innerHTML = "<a href=\"mailto:" + emailStr + "\">" + emailStr + "</a>";
};
