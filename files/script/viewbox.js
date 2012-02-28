
var svgdoc = null;
var target = null;
var targ_width, targ_height;
var orig_x, orig_y, orig_width, orig_height;
var drawing_width = 10000, drawing_height = 10000;
var start_x, start_y;
var dragActive = false;
var navActive = false;

var minimapWindow = null;
var minimap_x, minimap_y, minimap_width, minimap_height;


function updateOrig()
{
	var vb = target.getAttribute ("viewBox");
	if (vb)
	{
		var vba = vb.split(" "); //convert into array {x, y, width, height}
		orig_x = Number(vba[0]);
		orig_y = Number(vba[1]);
		orig_width = Number(vba[2]);
		orig_height = Number(vba[3]);
	}
}

function resizeTarget()
{
	target.setAttribute ("x", 0);
	target.setAttribute ("y", 0);
	target.setAttribute ("width", targ_width);
	target.setAttribute ("height", targ_height);
//	if (null == target.getAttribute ("viewBox"))
	{
		target.setAttribute ("viewBox", "0 0 " + targ_width +
			   							" " + targ_height);
	}
}

function getValInPx (aniLen)
{
	var len = aniLen.baseVal;
	len.convertToSpecifiedUnits (5);
	return len.valueInSpecifiedUnits;
}

function init(evt, targName)
{
	svgdoc = evt.target.ownerDocument;
	target = svgdoc.getElementById(targName);
	targ_width = getValInPx (evt.target.width);
	targ_height = getValInPx (evt.target.height);

	drawing_width = getValInPx (target.width);
	drawing_height = getValInPx (target.height);
	resizeTarget();
	updateOrig();
	initMinimap();
	updateMinimap (orig_x, orig_y);
}

function setSize (w, h)
{
	targ_width = w;
	targ_height = h;
	resizeTarget();
}

function captureMice (evt)
{
	start_x = evt.clientX;
	start_y = evt.clientY;
}

//onmousedown
function startDrag(evt)
{
	captureMice (evt);
	if (! navActive)
		dragActive = true;
}

//onmouseup
function stop(evt)
{
	updateOrig();
	navActive = false;
	dragActive = false;
}

//onmousemove
function drag(evt)
{
	x_mult = orig_width / targ_width;
	y_mult = orig_height / targ_height;
	if (dragActive)
		moveTarget (evt, x_mult, y_mult);
}

function initMinimap()
{
	var minimap = svgdoc.getElementById ("minimap");
	minimap.setAttribute("y", targ_height * 0.9);
	minimap.setAttribute("x", targ_width * 0.9);
	minimap.setAttribute("height", targ_height * 0.1);
	minimap.setAttribute("width", targ_width * 0.1);
	minimapWindow = svgdoc.getElementById("minimap-window");
	minimap_x = Number(minimap.getAttribute("x"));
	minimap_y = Number(minimap.getAttribute("y"));
	minimap_width = Number(minimap.getAttribute("width"));
	minimap_height = Number(minimap.getAttribute("height"));
}

function updateMinimap (x, y)
{
	var win_width = minimap_width * orig_width / drawing_width;
	var win_height = minimap_height * orig_height / drawing_height;
	var win_x = minimap_x + x * minimap_width / drawing_width;
	var win_y = minimap_y + y * minimap_height / drawing_height;

	minimapWindow.setAttribute ("x", win_x);
	minimapWindow.setAttribute ("y", win_y);
	minimapWindow.setAttribute ("width", win_width);
	minimapWindow.setAttribute ("height", win_height);
}

//minimap.onmousedown
function startNav (evt)
{
	captureMice (evt);
	dragActive = false;
	navActive = true;
}

//minimap.onmousemove
function doNav (evt)
{
	x_mult = - drawing_width / minimap_width;
	y_mult = - drawing_height / minimap_height;
	if (navActive)
		moveTarget (evt, x_mult, y_mult);
}

function moveTarget (evt, x_mult, y_mult)
{
	if (null != target)
	{
		cur_x = evt.clientX;
		cur_y = evt.clientY;
		norig_x = orig_x + (start_x - cur_x)*x_mult;
		norig_y = orig_y + (start_y - cur_y)*y_mult;

		if (norig_x > (drawing_width - orig_width))
		   	norig_x = drawing_width - orig_width;
		if (norig_y > (drawing_height - orig_height))
		   	norig_y = drawing_height - orig_height;
		if (norig_x < 0) norig_x = 0;
		if (norig_y < 0) norig_y = 0;

		target.setAttribute("viewBox", norig_x + " " + norig_y + " " +
									   orig_width + " " + orig_height);
		updateMinimap (norig_x, norig_y);
	}
}

