import React, { Component, Fragment } from 'react';
import { Tag, Tooltip, Input, Icon } from 'antd';
import isEqual from 'lodash/isEqual';
import cloneDeep from 'lodash/cloneDeep';
import PropTypes from 'prop-types';

class EditableTagGroup extends Component {
  static propTypes = {
    showAddBtn: PropTypes.bool,
    canClose: PropTypes.bool,
    value: PropTypes.array,
    // value: PropTypes.arrayOf(PropTypes.shape({
    //   color: PropTypes.string,
    //   name: PropTypes.string.isRequired,
    //   key:PropTypes.string.isRequired,
    // })),
  };

  static defaultProps = {
    showAddBtn: true,
    canClose: true,
    value: [],
  };

  static getDerivedStateFromProps(nextProps, curState) {
    if (
      Array.isArray(nextProps.value) &&
      !isEqual(nextProps.value, curState.tags) &&
      nextProps.onChange
    ) {
      return { tags: nextProps.value };
    }
    return null;
  }

  constructor(props) {
    super(props);
    this.state = {
      tags: props.value || [],
      inputVisible: false,
      inputValue: '',
    };
  }

  handleClose = removedTagKey => {
    const { onChange } = this.props;
    const { tags: oldtags } = this.state;
    const tags = oldtags.filter(item => item.key !== removedTagKey);
    this.setState({ tags });
    if (onChange) {
      onChange(tags);
    }
  };

  showInput = () => {
    this.setState({ inputVisible: true }, () => this.input.focus());
  };

  handleInputChange = e => {
    this.setState({ inputValue: e.target.value });
  };

  handleInputConfirm = () => {
    const { inputValue } = this.state;
    const { tags } = this.state;
    const { onChange } = this.props;

    const newTags = cloneDeep(tags);
    if (inputValue) {
      const newTag = { key: `${Math.random()}`, name: inputValue, color: '' };
      newTags.push(newTag);
    }
    if (onChange) {
      onChange(newTags);
    }

    this.setState({
      tags: newTags,
      inputVisible: false,
      inputValue: '',
    });
  };

  // eslint-disable-next-line no-return-assign
  saveInputRef = input => (this.input = input);

  render() {
    const { tags, inputVisible, inputValue } = this.state;
    const { showAddBtn, canClose } = this.props;

    const newTags = [];
    tags.forEach(item => {
      if (item && typeof item === 'string') {
        newTags.push({ key: item, name: item, color: '' });
      }
      if (item && item.key && item.name) {
        newTags.push({ key: item.key, name: item.name, color: item.color });
      }
      // return null;
    });

    return (
      <div>
        {newTags.map(item => {
          // const randomColor = `#${Math.floor(Math.random()*0xffffff).toString(16)}`
          const { key, name, color } = item;
          const isLongTag = name.length > 20;
          const tagElem = (
            <Tag key={key} color={color} closable={canClose} onClose={() => this.handleClose(key)}>
              {isLongTag ? `${name.slice(0, 20)}...` : name}
            </Tag>
          );
          return isLongTag ? (
            <Tooltip title={name} key={name}>
              {tagElem}
            </Tooltip>
          ) : (
            tagElem
          );
        })}
        {showAddBtn ? (
          <Fragment>
            {inputVisible && (
              <Input
                ref={this.saveInputRef}
                type="text"
                size="small"
                style={{ width: 78 }}
                value={inputValue}
                onChange={this.handleInputChange}
                onBlur={this.handleInputConfirm}
                onPressEnter={this.handleInputConfirm}
              />
            )}
            {!inputVisible && (
              <Tag onClick={this.showInput} style={{ background: '#fff', borderStyle: 'dashed' }}>
                <Icon type="plus" />
                标签
              </Tag>
            )}
          </Fragment>
        ) : null}
      </div>
    );
  }
}

export default EditableTagGroup;
