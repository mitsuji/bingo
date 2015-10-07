//
// admin.html 用 UIコントローラ
//
$(document).ready(function () {

    //
    // dialog object (Singleton)
    //
    var gameDialog = function( fOnSubmit ) {

	var divElem = $('#create-game-dialog');
	
	divElem.dialog({
	    autoOpen: false,
	    height: 400,
	    width: 400,
	    modal: true,
	    buttons: {
		'Create a Game': submit
	    }
	});
    
	divElem.find('form').on('submit', function( event ) {
	    event.preventDefault();
	    submit();
	});

	function submit() {
	    // validation
	    var publicKey = $('#newPublicKey').val();
	    var caption   = $('#newCaption').val(); 
	    fOnSubmit(publicKey,caption);
	}

	//
	// public methods
	//
	return {
	    open: function () {
		divElem.dialog('open');
	    },
	    close: function () {
		divElem.dialog('close');
	    }
	};
	
    }(onCreate);


    $('#addButton').on('click',function( event ){
	gameDialog.open();
    });

    
    function onCreate( publicKey, caption ) {
	$.post(
	    'http://' + location.host + '/add_game',
	    {public_key: publicKey, caption: caption},
	    function (data){
		if(data.success) {
		    gameDialog.close();

		    var gameKeys;
		    if(localStorage.getItem('gameKeys') != null){
			gameKeys = JSON.parse(localStorage.getItem('gameKeys'));
		    } else {
			gameKeys = {};
		    }

		    gameKeys[data.content.public_key] = data.content.secret_key;

		    localStorage.setItem('gameKeys',JSON.stringify(gameKeys));

		    load();
		    
		} else {
		    alert('error: ' + data.type);
		}
	    },
	    'json'
	);
    }
    

    function load() {
	
	jQuery.each($(".gameItem"),function(){
	    this.remove();
	})
	
	var gameKeys;
	if(localStorage.getItem('gameKeys') != null){
	    gameKeys = JSON.parse(localStorage.getItem('gameKeys'));
	} else {
	    gameKeys = {};
	}

	for (var publicKey in gameKeys ) {
	    var secretKey = gameKeys[publicKey];

	    $.post(
		'http://' + location.host + '/get_game',
		{secret_key: secretKey},
		function (data){
		    if(data.success) {
			onGame(data.content);
		    } else {
			alert('error: ' + data.type);
		    }
		},
		'json'
	    );
	}
	
    }

    function onGame( content ) {
	var item = $('#gameTemplate').clone(false);
	item.removeAttr('id');
	item.addClass('gameItem');
	item.css('display','table-row');
	item.find('.publicKey').text(content.public_key);
	item.find('.caption').text(content.caption);
	item.find('.launchButton').on('click',function(){
	    location.href = '/viewer.html?'+ content.public_key;
	});
	item.find('.deleteButton').on('click',function(){
	    alert("delete: " + content.secret_key);
	});
	$('#gameTable').append(item);
    }

    load();
    
});

