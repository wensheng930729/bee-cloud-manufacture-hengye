import React, { Component } from 'react';
import debounce from 'lodash/debounce';
// import PropTypes from 'prop-types';
import { Select, Spin, message } from 'antd';
import request from '../utils/request';

import { dealSearchEntityWithWrite } from './utils';

const { Option } = Select;

class RemoteSelect extends Component {
  static getDerivedStateFromProps(nextProps) {
    // clean state
    if (nextProps.value !== undefined) {
      return {
        value: nextProps.value,
      };
    }
    return null;
  }

  constructor(props) {
    super(props);
    this.lastFetchId = 0;
    this.fetchUser = debounce(this.fetchUser, 800);
    // this.lastRequest = debounce(this.lastRequest, 800)
  }

  state = {
    data: [],
    value: [],
    fetching: false,
  };

  fetchUser = oldValue => {
    const { searchParams, api, fieldId, method } = this.props;

    // 删除前后空格
    const value = oldValue.replace(/(^\s*)|(\s*$)/g, '');
    // 只能输入 中文/英文/数字
    const reg = /^[A-Za-z0-9-_\u4e00-\u9fa5]+$/;

    if (value.length === 0) {
      return;
    }

    if (!reg.test(value)) {
      message.warning('名称不符合规范');
      return;
    }

    if (!(typeof api === 'string' && api.length > 0 && fieldId !== undefined)) {
      message.error('传入的参数不正确');
      return;
    }

    const newSearchParams = { ...searchParams };
    newSearchParams[fieldId] = value;

    this.lastFetchId += 1;
    const fetchId = this.lastFetchId;
    // 重新请求时，将原来的数据清空
    this.setState({ data: [], fetching: true });

    async function lastRequest() {
      let response = await request(api, { params: newSearchParams });
      if (method && method === 'POST') {
        response = await request(api, { params: newSearchParams, method });
      }
      return response;
    }

    lastRequest()
      .then(body => {
        if (fetchId !== this.lastFetchId || !(body && body.result)) {
          // for fetch callback order
          return;
        }
        let data = [];

        if (body.result.success) {
          if (
            api === '/entity/api/searchEntityWithRead.json' &&
            body.result.value &&
            Array.isArray(body.result.value.data)
          ) {
            data = dealSearchEntityWithWrite({ data: body.result.value.data });
          } else if (Array.isArray(body.result.value) && body.result.value.length > 0) {
            if (api === '/hospitalKnowledgeFeedbackFacade/api/search.json') {
              data = body.result.value.map(item => {
                const newItem = {
                  value: item.id,
                  label: item.name,
                  origin: item,
                };
                return newItem;
              });
            } else {
              data = body.result.value.map(item => {
                if (item.ownerName && item.guid) {
                  const newGuid = item.guid.split('.')[2];
                  const newItem = {
                    value: newGuid,
                    label: newGuid,
                    origin: item,
                  };
                  return newItem;
                }
                return item;
              });
            }
          }
        } else if (body.result.errMessage) {
          message.error(body.result.errMessage);
        }

        setTimeout(() => {
          this.setState({ data, fetching: false });
        }, 1000);
      })
      .catch(err => {
        message.error(err);
      });
  };

  handleChange = value => {
    this.setState({
      // value,
      // data: [],
      fetching: false,
    });
    // eslint-disable-next-line no-unused-vars
    const { onChange, value: propsValue } = this.props;
    if (onChange) {
      onChange(value);
    }
    //  if(!propsValue||propsValue.length===0){
    //   this.setState({
    //     value,
    //   });
    // }
  };

  render() {
    const { fetching, data, value } = this.state;
    return (
      <Select
        // mode="tags"
        // labelInValue
        showSearch
        showArrow={false}
        value={value}
        placeholder="请输入要搜索的关键字"
        notFoundContent={fetching ? <Spin size="small" /> : null}
        filterOption={false}
        onSearch={this.fetchUser}
        onChange={this.handleChange}
        style={{ width: '100%' }}
      >
        {data.map(d => (
          <Option key={d.value} value={d.value}>
            {d.label}
          </Option>
        ))}
      </Select>
    );
  }
}

export default RemoteSelect;
