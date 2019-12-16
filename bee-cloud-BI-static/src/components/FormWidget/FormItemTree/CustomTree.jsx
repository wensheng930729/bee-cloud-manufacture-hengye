/* eslint-disable no-plusplus */
/* eslint-disable consistent-return */
import React, { Component } from 'react';
import { Tree, Input } from 'antd';
import { isEqual, debounce } from 'lodash';

const { TreeNode } = Tree;
const { Search } = Input;

// 数组扁平化
const generateList = (data, dataList = []) => {
  for (let i = 0; i < data.length; i++) {
    const node = data[i];
    dataList.push({ ...node });
    if (node.children) {
      generateList(node.children, dataList);
    }
  }
  return dataList;
};

// 获取父节点的key
const getParentKey = (key, tree) => {
  let parentKey;
  for (let i = 0; i < tree.length; i++) {
    const node = tree[i];
    if (node.children) {
      if (node.children.some(item => item.key === key)) {
        parentKey = node.key;
      } else if (getParentKey(key, node.children)) {
        parentKey = getParentKey(key, node.children);
      }
    }
  }
  return parentKey;
};

class SearchTree extends React.Component {
  static getDerivedStateFromProps(nextProps, prevState) {
    // 当传入的type发生变化的时候，更新state
    if (nextProps.treeData && !isEqual(nextProps.treeData, prevState.treeData)) {
      const dataList = generateList(nextProps.treeData);
      return {
        originTreeData: nextProps.treeData,
        treeData: nextProps.treeData,
        dataList,
      };
    }

    if (
      nextProps.expandedKeys &&
      nextProps.expandedKeys.length > 0 &&
      prevState.expandedKeys.length === 0
    ) {
      return { expandedKeys: nextProps.expandedKeys, autoExpandParent: false };
    }
    // 否则，对于state不进行任何操作
    return null;
  }

  constructor(props) {
    super(props);
    this.state = {
      // 树形数组
      treeData: [],
      // 一维数组,便于查询数据
      dataList: [],

      expandedKeys: props.expandedKeys || [],
      searchValue: '',
      autoExpandParent: true,
    };

    this.handleSelect = debounce(this.handleSelect, 800);
  }

  onExpand = expandedKeys => {
    this.setState({
      expandedKeys,
      autoExpandParent: false,
    });
  };

  onChange = e => {
    const { value } = e.target;
    const { dataList, treeData } = this.state;
    const expandedKeys = dataList
      .map(item => {
        if (item.title.indexOf(value) > -1) {
          return getParentKey(item.key, treeData);
        }
        return null;
      })
      .filter((item, i, self) => item && self.indexOf(item) === i);
    this.setState({
      expandedKeys,
      searchValue: value,
      autoExpandParent: true,
    });
  };

  handleSelect = selectedKeys => {
    const { onSelect } = this.props;
    if (onSelect) {
      onSelect(selectedKeys);
    }
  };

  render() {
    const { searchValue, expandedKeys, autoExpandParent, treeData } = this.state;

    const loop = data =>
      data.map(item => {
        const index = item.title.indexOf(searchValue);
        const beforeStr = item.title.substr(0, index);
        const afterStr = item.title.substr(index + searchValue.length);
        const title =
          index > -1 ? (
            <span>
              {beforeStr}
              <span style={{ color: '#f50' }}>{searchValue}</span>
              {afterStr}
            </span>
          ) : (
            <span>{item.title}</span>
          );
        if (item.children) {
          return (
            <TreeNode key={item.key} title={title}>
              {loop(item.children)}
            </TreeNode>
          );
        }
        return <TreeNode key={item.key} title={title} />;
      });
    return (
      <div>
        <Search style={{ marginBottom: 8 }} placeholder="Search" onChange={this.onChange} />
        <div style={{ height: '400px', overflowY: 'scroll' }}>
          <Tree
            onExpand={this.onExpand}
            expandedKeys={expandedKeys}
            autoExpandParent={autoExpandParent}
            showLine
            onSelect={this.handleSelect}
          >
            {loop(treeData)}
          </Tree>
        </div>
      </div>
    );
  }
}

export default SearchTree;
