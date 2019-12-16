import React, { Component } from 'react';
import { message, Spin } from 'antd';
import { connect } from 'dva';
import { FormItemRangePicker, FormItemSelect } from '@/components/FormWidget';

import { SearchFormHOC, TableForParentTab } from '@/components/TableForParentTab';

const params = {
  name: 'huhao',
};

const MyFormSearch = props => {
  const { getFieldDecorator, productsOptions, furnaceOptions } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="炉号"
        fieldId="furnaceId"
        required={false}
        selectProps={{ options: furnaceOptions }}
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

      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="日期范围"
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

@connect(({ ProductConsumptAnalyModel, ReportFormCommonModel, loading }) => ({
  furnaceOptions: ReportFormCommonModel.furnaceOptions,
  shiftCodeOptions: ReportFormCommonModel.shiftCodeOptions,
  productsOptions: ReportFormCommonModel.productsOptions,

  listSearchParams: ProductConsumptAnalyModel.listSearchParams,
  parentTabs: ProductConsumptAnalyModel.parentTabs,
  dataList: ProductConsumptAnalyModel.dataList,
  columns: ProductConsumptAnalyModel.columns,

  loading: loading.effects['ProductConsumptAnalyModel/getListEffect'],
}))
class Index extends Component {
  componentDidMount() {
    try {
      _czc1.push(['_trackEvent', '查看报表', '查看产品及消耗分析', '', '', '']);
    } catch (error) {}
    const defaultTabPagesParams = {
      type: 'output_and_consumption_analysis',
    };

    this.getTabPages(defaultTabPagesParams);
  }

  // 表单查询改变事件
  handleFormChange = field => {
    this.getTableData(field);
  };

  // 搜索按钮 点击事件
  handleSearch = () => {
    this.getTableData();
  };

  // tab切换事件
  handleTabChange = reportType => {
    const data = { reportType };
    this.handleFormChange(data);
  };

  // 表格分页事件
  handleTableChange = payload => {
    this.getTableData(payload);
  };

  // 是否能查看当前页面
  getTabPages = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'ProductConsumptAnalyModel/getTabPagesEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          this.getSearchOptions();
        }
      },
    });
  };

  // 获取查询条件中的 下拉框数据，并设置 默认查询条件
  getSearchOptions = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'ReportFormCommonModel/fetchFurnacesShiftCodeEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else if (status === 'ok') {
          const { furnaceOptions, shiftCodeOptions, productsOptions } = data;

          const newlistSearchParams = {
            productId: productsOptions[0].value,
            furnaceId: furnaceOptions[0].value,
            shiftCode: shiftCodeOptions[0].value,
          };

          this.handleFormChange(newlistSearchParams);
        }
      },
    });
  };

  // 获取表格数据
  getTableData = payload => {
    // 表格之前的查询条件
    const { dispatch } = this.props;

    dispatch({
      type: 'ProductConsumptAnalyModel/getListEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  render() {
    const {
      furnaceOptions,
      shiftCodeOptions,
      productsOptions,

      loading,
      listSearchParams,
      columns,
      dataList,
      parentTabs,
    } = this.props;

    const searchFormProps = {
      furnaceOptions,
      shiftCodeOptions,
      productsOptions,

      dataDetail: listSearchParams,
      handleSearch: this.handleSearch,
      onChange: this.handleFormChange,
    };

    const selfProps = {
      loading,
      columns,
      data: dataList,
      bordered: true,
      scroll: { x: 2000 },
      handleTabChange: this.handleTabChange,
      handleTableChange: this.handleTableChange,

      parentTabs,
      childTabs: null,
      currentTab: listSearchParams.reportType,

      ...searchFormProps,
    };
    if (parentTabs.length === 0) {
      return <h1 style={{ marginLeft: '40%' }}>请联系管理员开启当前页面</h1>;
    }
    return (
      <Spin spinning={loading}>
        <NewCom {...selfProps} />
      </Spin>
    );
  }
}

export default Index;
