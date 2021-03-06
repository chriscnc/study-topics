// tutorial1.js

var data = [
	{author: "Pete Hunt", text: "This is one comment"},
	{author: "Jordan Walke", text: "This is *another* comment"}
];

var CommentBox = React.createClass({
	render: function() {
		return (
			<div className="commentBox">
				<h1>Comments</h1>
				<CommentList data={this.props.data} />
				<CommentForm />
			</div>
		);
	}
});

var CommentList = React.createClass({
	render: function() {
		var commentNotes = this.props.data.map(function (comment) {
			return (
				<Comment author={comment.author}>
				  {comment.text}
				</Comment>
			);
		});
		return (
			<div className="commentList">
				{commentNodes}
			</div>
		);
	}
});

var CommentForm = React.createClass({
	render: function() {
		return (
			<div className="commentForm">
				Hello, world! I am a CommentForm.
			</div>
		);
	}
});

var converter = new Showdown.converter();
var Comment = React.createClass({
	render: function() {
		var rawMarkup = converter.makeHtml(this.props.children.toString());
		return (
			<div className="comment">
				<h2 className="commentAuthor">
					{this.props.author}
				</h2>
				<span dangerouslySetInnerHTML={{__html: rawMarkup}} />
			</div>
		);
	}
});

React.render(
		<CommentBox data={data} />,
		document.getElementById('content')
		);
