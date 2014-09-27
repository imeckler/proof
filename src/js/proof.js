$(function(){

function posRelativeToBody(node) {
  var pos = node.position();
  var bodyPos = $(document.body).position();
  return {left : pos.left - bodyPos.left, top : pos.top - bodyPos.top};
}

function panTo(node) {
  var pos = posRelativeToBody(node);
  var body = $(document.body);
  body.animate({
    'margin-left': -pos.left + 100,
    'margin-top': -pos.top + 100
  });
}
window.panTo = panTo;

function toggleSection(elt) {
  var open = elt.data('open');
}

function locationLabel(node) {
  var pos = node.attr('data-pos').split(',');
  var d = pos[0], i = pos[1];
  // TODO: Call Mathjax after this function
  return $('<div class="location-label">$' + '\\langle' + d + '\\rangle' + i + '$</div>');
}

function decorateStep(step) {
  var statement = step.children('.statement');

  var visToggle = $('<div class="toggle toggle-closed">');
  var wrapper   = $('<div class="step-wrapper">');
  var content   = $('<div class="node-content step-content">');
  var header    = $('<div class="step-header">');

  var visible = false;
  visToggle.click(function() {
    if (visible) {
      content.css('display', 'none');
    } else {
      content.css('display', 'block');
    }
    visToggle.toggleClass('toggle-closed');
    visToggle.toggleClass('toggle-open');
    visible = !visible;
  });

  header.append(statement);
  header.prepend(visToggle);
  content.append(step.contents());
  wrapper.append(header, content);

  step.replaceWith(wrapper);
  content.css('display', 'none');
}

function decorateSteps() {
  $('.step').each(function(i) {
    decorateStep($(this));
  });
}

function annotateWithNumbers(decls, level) {
  decls.each(function(i) {
  });
}

function decorateCaseBlocks(){
  $('.cases').each(function() {
    var block = $(this);
    block.prepend($('<div class="case-header">Cases:</div>'));
    // block.children('.case').each(function(i)
  });
}

function decorateProofs() {
  $('.proof').each(function() {
    var proof   = $(this);
    var wrapper = $("<div class='proof'></div>");
    var content = $('<div class="node-content"></div>');
    var header  = $('<div class="node-header">Proof:</div>');

    content.append(proof.contents());
    wrapper.append(header, content);
    proof.replaceWith(wrapper);

    content.toggleClass('closed');
    header.click(function() {
      content.toggleClass('closed');
    });
  });
}

function decorateLets() {
  $('.let').each(function() {
    var take = $(this);
    // var bindings = take.children('.bindings');
    var header = $('<div class="list-header">Let:</div>');
    take.prepend(header);
  })
}

function decorateTakes() {
  $('.take').each(function() {
    var take = $(this);
    // var bindings = take.children('.bindings');
    var header = $('<div class="list-header">Take:</div>');
    take.prepend(header);
  })
}

function decorateSuchThats() {
  $('.suchthat').each(function(){
    var suchThat = $(this);
    var header   = $('<div class="list-header">Such that:</div>');
    suchThat.prepend(header);
  });
}

function decorateSupposes() {
  $('.suppose').each(function() {
    var suppose = $(this);
    var assumptions = suppose.children('.assumptions');
    var results     = suppose.children('.results');
    assumptions.before($('<div class="list-header">Suppose:</div>'));
    results.before($('<div class="list-header">Then:</div>'));
  })
}

function wrapWithLocation(node) {
  var wrapper = $('<div class="wrapper">');
  node.replaceWith(wrapper);
  wrapper.append(locationLabel(node), node);
}

function decorateDefinitions() {
  $('.definition').each(function() {
    var defn    = $(this);
    var name    = defn.children('.name');
    var content = defn.children('.node-content');

    var header = $('<div class="decl-header">');
    defn.addClass('top-level');

    header.append($('<h2>Definition: </h2>'), name);
    defn.prepend(header);

    wrapWithLocation(defn);

    content.toggleClass('closed');
    header.click(function(){
      content.toggleClass('closed');
    });
  })
}

function decorateComments() {
  $('.comment').each(function() {
    var comment     = $(this);
    var nodeContent = comment.children('.node-content');
    var blockQuote  = $('<blockquote>');
    blockQuote.append(nodeContent.contents());
    nodeContent.append(blockQuote);
  });
}

function capitalize(s){
  return s.charAt(0).toUpperCase() + s.substring(1);
}

function decorateTheorems() {
  $('.theorem').each(function() {
    var theorem   = $(this);
    var name      = theorem.children('.name');
    var statement = theorem.children('.theorem-statement');
    var proof     = theorem.children('.proof');

    var header  = $('<div class="decl-header">');
    var content = $('<div class="node-content">');

    theorem.addClass('top-level');

    content.append(statement, proof);
    var title = capitalize(theorem.attr('data-thmkind'));
    header.append($('<h2>' + title + ': </h2>'), name);
    theorem.append(header, content);

    var wrapper = $('<div class="wrapper">');
    theorem.replaceWith(wrapper);
    wrapper.append(locationLabel(theorem), theorem);

    content.toggleClass('closed');
    header.click(function(){
      content.toggleClass('closed');
    });
  });
}

MathJax.Hub.Queue(decorateCaseBlocks);
MathJax.Hub.Queue(decorateSupposes);
MathJax.Hub.Queue(decorateTakes);
MathJax.Hub.Queue(decorateLets);
MathJax.Hub.Queue(decorateSuchThats);
MathJax.Hub.Queue(decorateDefinitions);
MathJax.Hub.Queue(decorateTheorems);
MathJax.Hub.Queue(decorateComments);
MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
MathJax.Hub.Queue(decorateProofs);
});
