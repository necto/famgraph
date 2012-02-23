
function removeChld (id)
{
	children = document.getElementById ("children");
	children.value = children.value.replace (id, "");
	childAlloc = document.getElementById ("chldAlloc" + id);
	childAlloc.parentNode.removeChild (childAlloc);
}
function addChld (id)
{
	childs = document.getElementById ("childs");
	children = document.getElementById ("children");
	
	children.value = children.value.replace(")", " " + id + ")");
	parent.window.request ("GET", "card" + id, function (xmlhttp)
	{
		var chldAlloc = document.createElement ('div');
		chldAlloc.innerHTML = xmlhttp.responseText;
		chldAlloc.setAttribute ("id", "chldAlloc" + id);
		var remChld = document.createElement ('div');
		remChld.setAttribute("onClick", "removeChld(" + id + ")");
		remChld.innerHTML = "remove";
		chldAlloc.appendChild (remChld);
		childs.appendChild (chldAlloc); 
		parent.window.resetDoWithCards();
	});
}
function AddParent (who, id)
{
	var container = document.getElementById ("manCont");
	document.getElementById (who).value = id;
	var set = document.getElementById (who + "Set");
	set.parentNode.removeChild (set);
	parent.window.request ("GET", "card" + id, function (xmlhttp)
	{
		var alloc = document.createElement ('div');
		alloc.innerHTML = xmlhttp.responseText;
		alloc.setAttribute ("id", who + "Alloc");
		var rem = document.createElement ('div');
		rem.setAttribute("onClick", "rem('" + who + "')");
		rem.innerHTML = "remove";
		var chg = document.createElement ('div');
		chg.setAttribute("onClick", "change('" + who + "', " + who + "Add)");
		chg.innerHTML = "change";
		alloc.appendChild (rem);
		alloc.appendChild (chg);
		container.appendChild (alloc);
		parent.window.resetDoWithCards();
	});
}
function manAdd (id)
{
	AddParent ("man", id);
}
function wifeAdd (id)
{
	AddParent ("wife", id);
}
function rem(who)
{
	it = document.getElementById (who + "Alloc");
	par = it.parentNode;
	par.removeChild (it);
	set = document.createElement ('div');
	set.setAttribute ("onClick",
					  "parent.window.setDoWithCards(" + who + "Add);");
	set.setAttribute ("id", who + "Set");
	set.innerHTML = "set " + who;
	par.appendChild (set);
	document.getElementById (who).value = null;
}
function change(who, func)
{
	rem (who);
	parent.window.setDoWithCards (func);
}
