import React, { Component } from 'react';
import { message } from 'antd';
import router from 'umi/router';

import { connect } from 'dva';
import { FormItemRangePicker, FormItemSelect, FormItemInputSearch } from '@/components/FormWidget';

import { SearchFormHOC, TableForParentTab } from '@/components/TableForParentTab';
import * as dictionary from '@/constants/dictionary';

const params = {
  name: 'huhao',
};

const MyFormSearch = props => {
  const { getFieldDecorator, productsOptions } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="contractNum"
        required={false}
        inputProps={{ placeholder: '按合同编号进行查询' }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="产品名称"
        fieldId="productId"
        required={false}
        selectProps={{ options: productsOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="合同签订日期"
        fieldId="dateRange"
        required={false}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
        formItemLayout={formItemLayout}
      />
    </>
  );
};

const NewIndex = SearchFormHOC(params)(MyFormSearch);

const NewCom = TableForParentTab(params)(NewIndex);

@connect(({ purchase, loading }) => ({
  productsOptions: purchase.productsOptions,

  listSearchParams: purchase.listSearchParams,

  dataObj: purchase.dataObj,
  loading: loading.effects['purchase/getListEffect'],
}))
class Index extends Component {
  state = {
    parentTabs: [
      {
        cName: '未完成',
        key: '1',
        value: '0',
      },
      {
        cName: '已完成',
        key: '2',
        value: '1',
      },
    ],
    selfKey: undefined,
    currentTabKey: '1',
  };

  componentDidMount() {
    const { dispatch } = this.props;
    dispatch({
      type: 'purchase/getProductsEffect',
      callback: (a, b, c) => {
        if (b === 'ok') {
          this.getSearchOptions({ productsOptions: a });
        }
      },
    });
  }

  // 表单查询改变事件
  handleFormChange = field => {
    const { dispatch } = this.props;
    dispatch({
      type: 'purchase/cacheSearchParamsReduce',
      payload: field,
    });
  };

  // 搜索按钮 点击事件
  handleSearch = () => {
    this.getTableData({}, this.state.selfKey);
  };

  // tab切换事件
  handleTabChange = key => {
    this.handleFormChange();
    if (key === '1') {
      this.getTableData({}, this.state.parentTabs[0].value);
      this.setState({
        selfKey: '0',
        currentTabKey: key,
      });
    } else {
      this.getTableData({}, this.state.parentTabs[1].value);
      this.setState({
        selfKey: '1',
        currentTabKey: key,
      });
    }
  };

  // 表格分页事件
  handleTableChange = payload => {
    this.getTableData({ payload }, this.state.selfKey);
  };

  // 获取查询条件中的 下拉框数据，并设置 默认查询条件
  getSearchOptions = payload => {
    const { productsOptions } = payload;
    const { listSearchParams } = this.props;

    const newlistSearchParams = {
      ...listSearchParams,
      productId: productsOptions[0].value,
      completed: '0',
      contractNum: '', // 合同编号
    };
    this.handleFormChange(newlistSearchParams);
    this.getTableData({ payload: newlistSearchParams });
  };

  // 获取表格数据
  getTableData = (obj = {}, key = '0') => {
    const newObj = {
      payload: obj.payload || {},
    };
    // 默认降序
    const sort = { sort: 'desc' };
    // 表格之前的查询条件
    const { listSearchParams, dispatch } = this.props;
    const listSearchParamsCopy = { ...listSearchParams, ...{ completed: key } };
    const listSearchParamsSelf = { ...listSearchParamsCopy, ...newObj.payload };
    // 日期参数格式化
    let selfDateRange = null;
    if (listSearchParamsSelf.dateRange) {
      selfDateRange = { ...listSearchParamsSelf.dateRange };
    } else {
      selfDateRange = {
        startTime: listSearchParamsSelf.startTime,
        endTime: listSearchParamsSelf.endTime,
      };
    }
    const {
      productId,
      currentPage,
      pageSize,
      searchCount,
      contractNum,
      completed,
    } = listSearchParamsSelf;
    const payload = {
      currentPage,
      pageSize,
      searchCount,
      contractNum,
      productId,
      completed,
      dateRange: selfDateRange,
      ...sort,
      ...selfDateRange,
      // dateRange: selfDateRange,
    };
    dispatch({
      type: 'purchase/getListEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  handleFormAdd = () => {
    router.push('/purchase/order/add');
  };

  // 跳转详情
  handleCheckDetail = row => {
    try {
      _czc1.push(['_trackEvent', '采购订单', '查看详情', '', '', '']);
    } catch (error) {}
    router.push(`/purchase/order/detail?contractBusinessId=${row.contractBusinessId}`);
  };

  render() {
    const { parentTabs, currentTabKey } = this.state;
    if (parentTabs.length === 0) {
      return <h1 style={{ marginLeft: '40%' }}>请联系管理员开启当前页面</h1>;
    }
    const {
      productsOptions,

      loading,
      listSearchParams,
      dataObj,
    } = this.props;

    const searchFormProps = {
      productsOptions,

      dataDetail: listSearchParams,
      handleSearch: this.handleSearch,
      onChange: this.handleFormChange,
    };
    const columns = [
      {
        title: '采购合同号',
        dataIndex: 'contractNum',
        key: 'contractNum',
      },
      {
        title: '供应商',
        dataIndex: 'supplierName',
        key: 'supplierName',
      },
      {
        title: '合同数量',
        dataIndex: 'quantity',
        key: 'quantity',
      },
      {
        title: '合同金额',
        dataIndex: 'amount',
        key: 'amount',
      },
      {
        title: '合同状态',
        dataIndex: 'completed',
        key: 'completed',
        render: text => <span>{dictionary.completed[text]}</span>,
      },
      {
        title: '合同签订日期',
        dataIndex: 'signDate',
        key: 'signDate',
      },
      {
        title: '操作',
        dataIndex: 'operation',
        key: 'operation',
        render: (text, record) => (
          <span>
            <a onClick={() => this.handleCheckDetail(record)}>查看详情</a>
          </span>
        ),
      },
    ];
    const selfProps = {
      loading,
      columns,
      data: dataObj || {
        list: [],
        pagination: {},
      },
      bordered: false,
      handleTabChange: this.handleTabChange,
      handleTableChange: this.handleTableChange,
      handleBtnAdd: true,
      handleBtnAddText: '+新增合同', // 新增按钮
      handleFormAdd: this.handleFormAdd,
      currentTab: currentTabKey,

      parentTabs,
      childTabs: null,

      ...searchFormProps,
    };
    return <NewCom {...selfProps} />;
  }
}

export default Index;
