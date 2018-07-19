
$(document).on('click', '.caf-nav__item', function () {
        Shiny.onInputChange('last_click',this.id);
   });

$(document).on('click', '.barType', function () {
        Shiny.onInputChange('last_btn',this.id);
   });   

$(document).ready(function(){
var tabManagerCollection = document.querySelectorAll('.tab-manager')
var tabSectionCollection = document.querySelectorAll('.tab-section')

var tabManager = Array.prototype.slice.call(tabManagerCollection)
var tabSection = Array.prototype.slice.call(tabSectionCollection)

console.log(tabManager)
console.log(tabSection)

tabManager.forEach(function (tab) {
  tab.addEventListener('click', function (event) {
    event.preventDefault()
    $(tabManager).removeClass('caf-nav__item--active')
    event.target.classList.add('caf-nav__item--active')
    var id = event.target.dataset.section;
    tabSection.forEach(function (t) {
      t.classList.add('no-visible')
    })
    document.getElementById(id).classList.remove('no-visible')
  })
})
})



