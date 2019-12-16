import React, { Component } from 'react';
import { Card, Form, Button, message } from 'antd';
import { connect } from 'dva';
import router from 'umi/router';
import {
  FormItemInputSearch,
  FormItemSelect,
  FormItemRangePicker,
  CustomFormHOC,
} from '@/components/FormWidget';
import StandardTable from '@/components/FormWidget/StandardTable';

import styles from './index.less';
import { businessTypeOptions } from '@/consts/reportForm';

const SearchFrom = props => {
  const {
    handleSearch,
    form: { getFieldDecorator },
    productsOptions,
  } = props;

  // 查询按钮 点击事件
  const handleSelefSearch = e => {
    e.preventDefault();
    if (handleSearch) {
      handleSearch();
    }
  };

  const formItemLayout = {};

  return (
    <Form layout="inline" onSubmit={handleSelefSearch}>
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="contractNum"
        required={false}
        inputProps={{ placeholder: '按采购/销售订单编号搜索' }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="产品"
        fieldId="productId"
        required={false}
        selectProps={{ options: productsOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="合同类型"
        fieldId="businessType"
        required={false}
        selectProps={{ options: businessTypeOptions }}
        formItemLayout={formItemLayout}
      />

      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="创建日期"
        fieldId="dateRange"
        required={false}
        formItemLayout={formItemLayout}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
        dateFormat="YYYY-MM-DD"
      />
      <Form.Item
        wrapperCol={{
          xs: { span: 24, offset: 0 },
          sm: { span: 16, offset: 8 },
        }}
      >
        <Button type="primary" htmlType="submit">
          查询
        </Button>
      </Form.Item>
    </Form>
  );
};

const NewSearchForm = CustomFormHOC(SearchFrom);

@connect(({ LogisticsOrder, loading }) => ({
  productsOptions: LogisticsOrder.productsOptions,

  listSearchParams: LogisticsOrder.listSearchParams,

  dataObj: LogisticsOrder.dataObj,
  loading: loading.effects['LogisticsOrder/getListEffect'],
}))
export default class Index extends Component {
  componentDidMount() {
    const { dispatch } = this.props;
    dispatch({
      type: 'LogisticsOrder/getProductsEffect',
      callback: (a, b, c) => {
        if (b === 'ok') {
          this.getSearchOptions({ productsOptions: a });
        }
      },
    });
  }

  // 搜索按钮 点击事件
  handleSearch = () => {
    this.getTableData({ payload: { currentPage: 1, pageSize: 10 } });
  };

  // 表单查询 改变事件
  handleFormChange = field => {
    const { dispatch } = this.props;
    dispatch({
      type: 'LogisticsOrder/cacheSearchParamsReduce',
      payload: field,
    });
    // const { handleFormChange } = this.props;
    // if (handleFormChange) {
    //   handleFormChange(field);
    // }
  };

  // 分页改变事件
  handleTableChange = payload => {
    const params = {
      currentPage: payload.current,
      pageSize: payload.pageSize,
    };

    this.getTableData({ payload: params });
  };

  // 获取查询条件中的 下拉框数据，并设置 默认查询条件
  getSearchOptions = payload => {
    const { productsOptions } = payload;
    const { listSearchParams } = this.props;

    const newlistSearchParams = {
      ...listSearchParams,
      productId: productsOptions[0].value,
      businessType: businessTypeOptions[0].value, // 合同类型 1采购 2销售
      contractNum: '', // 合同编号
    };
    this.handleFormChange(newlistSearchParams);
    this.getTableData({ payload: newlistSearchParams });
  };

  // 查看详情
  handleCheckDetail = payload => {
    try {
      _czc1.push(['_trackEvent', '物流订单', '查看详情', '', '', '']);
    } catch (error) {}
    const { contractBusinessId, businessType } = payload;
    router.push({
      pathname: '/logisticsManage/order/detail',
      query: {
        contractBusinessId,
        businessType,
      },
    });
  };

  // 获取表格数据
  getTableData = (obj = {}) => {
    const newObj = {
      payload: obj.payload || {},
    };
    // 默认降序
    const sort = { sort: 'desc' };
    // 表格之前的查询条件
    const { listSearchParams, dispatch } = this.props;
    const listSearchParamsSelf = { ...listSearchParams, ...newObj.payload };

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
      businessType,
    } = listSearchParamsSelf;
    const params = {
      currentPage,
      pageSize,
      searchCount,
      contractNum,
      businessType,
      productId,
      ...sort,
      ...selfDateRange,
      dateRange: selfDateRange,
    };
    dispatch({
      type: 'LogisticsOrder/getListEffect',
      payload: params,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  render() {
    const { productsOptions, listSearchParams, dataObj } = this.props;

    const columns = [
      {
        title: '业务合同编号',
        dataIndex: 'contractNum',
        key: 'contractNum',
      },
      {
        title: '合同类型',
        dataIndex: 'businessTypeName',
        key: 'businessTypeName',
      },
      {
        title: '产品',
        dataIndex: 'productName',
        key: 'productName',
      },
      {
        title: '合同数量',
        dataIndex: 'quantity',
        key: 'quantity',
      },
      {
        title: '起点',
        dataIndex: 'originAddress',
        key: 'originAddress',
      },
      {
        title: '终点',
        dataIndex: 'endAddress',
        key: 'endAddress',
      },
      {
        title: '创建日期',
        dataIndex: 'createTime',
        key: 'createTime',
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
    const tableProps = {
      loading: false,
      columns,
      data: dataObj,
      onChange: this.handleTableChange,
      bordered: false,
      scroll: {},
    };
    const SearchFromProps = {
      dataDetail: listSearchParams,
      onChange: this.handleFormChange,
      productsOptions,
      handleSearch: this.handleSearch,
    };

    return (
      <Card className={styles.cardContainer}>
        <NewSearchForm {...SearchFromProps} />
        <StandardTable {...tableProps} />
      </Card>
    );
  }
}
