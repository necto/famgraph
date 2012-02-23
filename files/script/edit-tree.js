
function request (method, str, onAnswer)
{
	var xmlhttp=new XMLHttpRequest();
	xmlhttp.onreadystatechange = function ()
	{
		if (xmlhttp.readyState==4 && xmlhttp.status==200)
		{
			onAnswer(xmlhttp);
		}
	}
	xmlhttp.open(method, str, true);
	xmlhttp.send();
}
function showPerson (id)
{
	document.getElementById("right-panel").src = "person" + id;
}
window.doWithCards = showPerson;
function setDoWithCards (fun)
{
	document.getElementById("tree").className += ' active';
	window.doWithCards = fun;
}
function removeClass (element, cl)
{
	element.className = element.className.replace (cl, "");
}
function resetDoWithCards()
{
	removeClass (document.getElementById("tree"), "active");
	window.doWithCards = showPerson;
}
function cardClicked (id)
{
	window.doWithCards (id);
}
function marrClicked (id)
{
	resetDoWithCards();
	document.getElementById("right-panel").src = "wedding" + id;
}
function refreshTree()
{
	resetDoWithCards();
	document.getElementById("tree").data = "tree";
}
function refreshNode(id, text)
{
	alert(	document.getElementById("tree").contentDocument.getElementById("card1").innerHtml);
}
